import pandas as pd
import numpy as np
from itertools import product
from itables import init_notebook_mode, show
import requests
from bs4 import BeautifulSoup
import yaml
import io
import base64
from IPython.display import HTML, display
from plotly import express as px
import auto_invest as ai

with open(r'../config.yaml', encoding='UTF-8') as f:
    cfg = yaml.load(f, Loader=yaml.FullLoader)

def custom_float_format(x):
    if x == int(x):  # 실제로 소수점 아래 값이 0인 경우
        return '{:,.0f}'.format(x)
    else:  # 소수점 아래 값이 0이 아닌 경우
        return '{:,.2f}'.format(x)

def inf_to_nan(sr):
    return sr.replace([np.inf, -np.inf], np.nan)

def matplot_to_html(fig, left_px=10):
    buffer = io.BytesIO()
    fig.savefig(buffer, format='png')
    plt.close(fig)
    plot_data = base64.b64encode(buffer.getvalue()).decode()

    return f'<div  style="margin-left: {left_px}px;"><img src="data:image/png;base64,{plot_data}"/></div>'

def df_to_html(df, left_px=10):
    return f'<div  style="margin-left: {left_px}px;">{df.to_html()}</div>'

def display_inline(*args):
    htmls = list(args)
    html = ''.join(htmls)
    html = f'<div style="display: flex; justify-content: center;">' + html + '</div>'
    return HTML(html)

# 옵션 설정
pd.options.display.float_format = custom_float_format

YEAR = cfg['year']
DAYS = pd.date_range(start=f'{YEAR}-01-01', end=f'{YEAR}-12-31', freq='D')

assets = pd.read_excel('trade.xlsx',sheet_name=0)
usd1 = pd.read_excel('trade.xlsx',sheet_name=1)
usd2 = pd.read_excel('trade.xlsx',sheet_name=2)
fi = pd.read_excel('trade.xlsx',sheet_name=3)
krw1 = pd.read_excel('trade.xlsx',sheet_name=4)
krw2 = pd.read_excel('trade.xlsx',sheet_name=5)
krw3 = pd.read_excel('trade.xlsx',sheet_name=6)
krw4 = fi.drop('달러입출금', axis=1).copy()

res = requests.get("http://finance.naver.com/marketindex/").content
soup = BeautifulSoup(res, "html.parser")
ex = float(soup.select_one("div.head_info > span.value").string.replace(',',''))

bl = ai.AutoInvest("boolio")
price = bl.inquire_balance_ovs()[['종목코드','평가금액']].astype({'평가금액':'float'})

def get_asset_returns(cur):

    def get_bs_pl(cur):

        as_list = pd.DataFrame(
            product(assets.loc[assets.통화==cur,'종목코드'], DAYS),
            columns=['종목코드', '거래일자'])

        data = {'달러': [usd1, usd2],
                '원화': [krw1, krw2, krw3, krw4]}
        
        trade = as_list \
            .merge(
                pd.concat(data[cur]),
                on=['종목코드', '거래일자'],
                how='left'
            ).fillna(0) \
             .sort_values(['종목코드', '거래일자']) \
             .reset_index(drop=True)
        
        trade['장부금액'] = trade.groupby('종목코드').apply(lambda x: (x.매입액 - x.매도원금).cumsum()).reset_index(drop=True)
        trade['보유수량'] = trade.groupby('종목코드').apply(lambda x: (x.매입수량 - x.매도수량).cumsum()).reset_index(drop=True)
        trade['평잔'] = trade.groupby('종목코드')['장부금액'].expanding().mean().reset_index(drop=True)
        trade['수익'] = trade.매매수익 + trade.이자배당액
        trade['비용'] = trade.매입비용 + trade.매도비용
        trade['실현손익'] = trade.수익 - trade.비용
        
        if cfg['today'] is None:
            today=pd.Timestamp.today().normalize()
        else:
            today=pd.to_datetime(cfg['today'])
        
        bs_pl = \
            assets.loc[assets.통화 == cur, ['종목코드', '종목명', '통화']]\
              .merge(price, on='종목코드', how='left')\
              .merge(trade.loc[trade['거래일자'] == today, ['종목코드', '보유수량', '장부금액', '평잔']],
                     on='종목코드', how='left')\
              .merge(trade[trade['거래일자'] <= today]\
                     .groupby('종목코드').agg({
                        '수익': 'sum',
                        '비용': 'sum',
                        '실현손익': 'sum'
                     }).reset_index(),
                      on='종목코드', how='left').copy()
        
        if cur == '달러':
            cash = trade \
                .merge(assets[['종목코드','계좌']], on='종목코드', how='left') \
                .groupby(['거래일자','계좌']) \
                .apply(lambda x: (x.현금수입 + x.입출금 - x.현금지출).sum()) \
                .unstack(level=1)
        
            bs_pl.loc[bs_pl.종목명.str.contains('달러'), '장부금액'] = \
                cash.cumsum().loc[today].values
            bs_pl.loc[bs_pl.종목명.str.contains('달러'), '평잔'] = \
                cash.expanding().mean().loc[today].values
        
        elif cur == '원화':
            bs_pl.loc[bs_pl.종목명 == '롯데케미칼', '평가금액'] = \
                int(bl.get_current_price("011170")['stck_prpr'])*70
        
        bs_pl['평가금액'] = bs_pl.평가금액.mask(bs_pl.평가금액.isnull(), bs_pl.장부금액)
        
        return bs_pl.copy()

    usd = get_bs_pl('달러')
    krw = get_bs_pl('원화')

    if cur == '원화':
        krw.loc[krw.종목명=='달러자산','평가금액'] = round(usd.평가금액.sum()*ex, 0)
        bs_pl = krw.copy()
    else :
        bs_pl = usd.copy()

    bs_pl['평가손익'] = bs_pl.평가금액 - bs_pl.장부금액
    bs_pl['총손익'] = bs_pl.실현손익 + bs_pl.평가손익

    bs_pl['실현수익률'] = inf_to_nan(bs_pl.실현손익 / bs_pl.평잔 * 100)
    bs_pl['운용수익률'] = inf_to_nan(bs_pl.총손익 / bs_pl.평잔 * 100)

    bs_pl = bs_pl.merge(assets[['종목명', '자산군', '세부자산군']], on='종목명', how='left')

    return bs_pl.copy()

usd = get_asset_returns('달러')
krw = get_asset_returns('원화')

def allo(level = 1):
    
    df = pd.concat([
            usd.assign(
                평가금액 = round(usd.평가금액 * ex,0)
            ),krw], 
            ignore_index=True)\
        .groupby(['자산군','세부자산군','통화'])[['평가금액']]\
        .sum().drop('외화자산')\
        .assign(
            투자비중 = lambda x: x.평가금액 / x.평가금액.sum() * 100
        )
        
    if level==0:
        return df.reset_index(level=2, drop=True).groupby('자산군').sum()
    
    elif level==1:
        df.loc[('합계','',''),:] = [df.평가금액.sum(),100]
        df['투자비중(자산군별)'] = df.groupby('자산군')['평가금액'].apply(lambda x: x/x.sum()*100)
        return df
    else:    
        df = df.reorder_levels([2,0,1]).reset_index(level=2, drop=True).sort_index(level=0)
        df = df.groupby(['통화','자산군']).sum()
        df.loc[('합계',''),:] = [df.평가금액.sum(),100]
        df['투자비중(통화별)'] = df.groupby('통화')['평가금액'].apply(lambda x: x/x.sum()*100)
        return df

plot_allo = display_inline(
    df_to_html(allo(0)), 
    df_to_html(allo(1), 50),
    df_to_html(allo(2), 50)
)

plot_pie = px.sunburst(
    allo(1).투자비중.iloc[:-1].reset_index(), 
    path=['자산군','세부자산군'], values='투자비중', 
    width=500, height=500
)


def get_class_returns():
    
    def class_returns(asset_returns):
        class_returns = asset_returns\
            .drop(['종목명', '통화', '실현수익률', '운용수익률'], axis=1)\
            .groupby(['자산군', '세부자산군']).sum()
    
        cur = asset_returns.통화[1]
        class_returns.loc[(f'{cur}전체',''), :] = class_returns.sum().values
        class_returns['실현수익률'] = inf_to_nan(class_returns.실현손익 / class_returns.평잔 * 100)
        class_returns['운용수익률'] = inf_to_nan(class_returns.총손익 / class_returns.평잔 * 100)
    
        return class_returns
    
    krw_ret = class_returns(krw)
    usd_ret = class_returns(usd)
    
    return pd.concat([krw_ret,usd_ret])

class_table = get_class_returns()
asset_table = pd.concat([krw,usd])