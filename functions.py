import pandas as pd
import numpy as np
from bs4 import BeautifulSoup
import requests
import json
import yaml
import os
from datetime import datetime
from collections import namedtuple
from itertools import product
from itables import init_notebook_mode
from plotly import express as px
from PublicDataReader import Ecos
from collections import namedtuple


def get_config():

    with open(r'config.yaml', encoding='UTF-8') as f:
        cfg = yaml.load(f, Loader=yaml.FullLoader)

    return cfg


def get_result_object(json_data):
    _tc_ = namedtuple('res', json_data.keys())

    return _tc_(**json_data)


class AutoInvest:

    def __init__(self, account="my"):
        cfg = get_config()
        self.token_tmp = "KIS" + account
        if not os.path.exists(self.token_tmp):
            f = open(self.token_tmp, "w+")

        self.APP_KEY = cfg[account+'_app']
        self.APP_SECRET = cfg[account+'_sec']
        self.ACCT = cfg[account + '_acct']
        self.URL_BASE = cfg['prod']
        self.MY_AGENT = cfg['agent']
        self.base_headers = {
            "Content-Type": "application/json",
            "Accept": "text/plain",
            "charset": "UTF-8",
            'User-Agent': self.MY_AGENT
        }
        self.token_headers = {
            **self.base_headers,
            "authorization": f"Bearer {self.auth()}",
            "appkey": self.APP_KEY,
            "appsecret": self.APP_SECRET,
        }

    def save_token(self, my_token, my_expired):
        valid_date = datetime.strptime(my_expired, '%Y-%m-%d %H:%M:%S')
        print('Save token date: ', valid_date)
        with open(self.token_tmp, 'w', encoding='utf-8') as f:
            f.write(f'token: {my_token}\n')
            f.write(f'valid-date: {valid_date}\n')

    def read_token(self):
        try:
            # 토큰이 저장된 파일 읽기
            with open(self.token_tmp, encoding='UTF-8') as f:
                tkg_tmp = yaml.load(f, Loader=yaml.FullLoader)

            # 토큰 만료 일,시간
            exp_dt = datetime.strftime(tkg_tmp['valid-date'], '%Y-%m-%d %H:%M:%S')
            # 현재일자,시간
            now_dt = datetime.today().strftime("%Y-%m-%d %H:%M:%S")

            # 저장된 토큰 만료일자 체크 (만료일시 > 현재일시 인경우 보관 토큰 리턴)
            if exp_dt > now_dt:
                return tkg_tmp['token']
            else:
                print('Need new token: ', tkg_tmp['valid-date'])
                return None

        except Exception as e:
            print('read token error: ', e)
            return None

    def auth(self):

        body = {"grant_type": "client_credentials",
                "appkey": self.APP_KEY,
                "appsecret": self.APP_SECRET}

        # 기존 발급된 토큰이 있는지 확인
        saved_token = self.read_token()  # 기존 발급 토큰 확인

        if saved_token is None:  # 기존 발급 토큰 확인이 안되면 발급처리
            path = "oauth2/tokenP"
            url = f"{self.URL_BASE}/{path}"
            res = requests.post(url, data=json.dumps(body),
                                headers=self.base_headers)  # 토큰 발급
            rescode = res.status_code
            if rescode == 200:  # 토큰 정상 발급
                my_token = get_result_object(res.json()).access_token  # 토큰값 가져오기
                my_expired = get_result_object(res.json()).access_token_token_expired  # 토큰값 만료일시 가져오기
                self.save_token(my_token, my_expired)  # 새로 발급 받은 토큰 저장
            else:
                print('Get Authentification token fail!\nYou have to restart your app!!!')
                return
        else:
            my_token = saved_token  # 기존 발급 토큰 확인되어 기존 토큰 사용

        return my_token

    def hashkey(self, datas):
        path = "uapi/hashkey"
        url = f"{self.URL_BASE}/{path}"
        headers = {
            **self.base_headers,
            'appKey': self.APP_KEY,
            'appSecret': self.APP_SECRET,
        }
        res = requests.post(url, headers=headers, data=json.dumps(datas))
        hashkey = res.json()["HASH"]

        return hashkey

    def inquire_account_balance(self):

        path = "uapi/domestic-stock/v1/trading/inquire-account-balance"
        url = f"{self.URL_BASE}/{path}"
        data = {
            "CANO": self.ACCT,
            "ACNT_PRDT_CD": "01",
            "INQR_DVSN_1": "",
            "BSPR_BF_DT_APLY_YN": ""
        }
        headers = {**self.token_headers,
                   "tr_id": "CTRP6548R",
                   "custtype": "P"}
        res = requests.get(url, headers=headers, params=data)
        df1 = pd.DataFrame(res.json()['output1'])
        df1.columns = ['매입금액', '평가금액', '평가손익', '신용대출', '순자산', '비중']
        asset = "주식 펀드_MMW 채권 ELS_DLS WRAP 신탁_퇴직연금_외화신탁 RP_발행어음 해외주식 해외채권 금현물 " \
                "CD_CP 단기사채 타사상품 외화단기사채 외화ELS_DLS 외화 예수금+CMA 청약자예수 합계".split()
        df1.insert(0, "자산구분", asset)
        df1[df1.columns[1:]] = df1[df1.columns[1:]].apply(pd.to_numeric, errors='coerce')

        return df1[df1.비중 != 0].reset_index(drop=True)

    def inquire_balance(self):

        path = "/uapi/domestic-stock/v1/trading/inquire-balance"
        url = f"{self.URL_BASE}/{path}"
        data = {
            "CANO": self.ACCT,
            "ACNT_PRDT_CD": "01",
            "AFHR_FLPR_YN": "N",
            "OFL_YN": "N",
            "INQR_DVSN": "01",
            "UNPR_DVSN": "01",
            "FUND_STTL_ICLD_YN": "N",
            "FNCG_AMT_AUTO_RDPT_YN": "N",
            "PRCS_DVSN": "01",
            "CTX_AREA_FK100": "",
            "CTX_AREA_NK100": ""
        }
        headers = {**self.token_headers,
                   "tr_id": "TTTC8434R",
                   "custtype": "P"}
        res = requests.get(url, headers=headers, params=data)
        df1 = pd.DataFrame(res.json()['output1'])[['pdno', 'prdt_name', 'evlu_amt']].copy()
        df1.columns = ['종목코드', '상품명', '평가금액']

        return df1

    def inquire_balance_ovs(self, cur='USD'):

        path = "uapi/overseas-stock/v1/trading/inquire-balance"
        url = f"{self.URL_BASE}/{path}"
        exc = {"USD":"NASD", "JPY":"TKSE"}
        data = {
            "CANO": self.ACCT,
            "ACNT_PRDT_CD": "01",
            "OVRS_EXCG_CD": exc[cur],
            "TR_CRCY_CD": cur,
            "CTX_AREA_FK200": "",
            "CTX_AREA_NK200": ""
        }
        headers = {**self.token_headers,
                   "tr_id": "TTTS3012R",
                   "custtype": "P"}
        res = requests.get(url, headers=headers, params=data)
        df1 = pd.DataFrame(res.json()['output1'])[['ovrs_pdno', 'ovrs_item_name', 'ovrs_stck_evlu_amt']].copy()
        df1.columns = ['종목코드', '상품명', '평가금액']

        return df1

    def get_current_price(self, sym_cd):

        path = "/uapi/domestic-stock/v1/quotations/inquire-price"
        url = f"{self.URL_BASE}/{path}"
        data = {
            "FID_COND_MRKT_DIV_CODE": "J",
            "FID_INPUT_ISCD": sym_cd
        }
        headers = {**self.token_headers,
                   "tr_id": "FHKST01010100",
                   "custtype": "P"}
        res = requests.get(url, headers=headers, params=data)

        return pd.Series(res.json()['output'])


def get_exchange_rate(cur = '달러'):

    res = requests.get("http://finance.naver.com/marketindex/").content
    soup = BeautifulSoup(res, "html.parser")

    num = {'달러' : 0, '엔' : 1, '유로' : 2, '위안' : 3}

    return float(soup.select("div.head_info > span.value")[num[cur]].string.replace(',', ''))

def custom_float_format(x):
    if x == int(x):  # 실제로 소수점 아래 값이 0인 경우
        return '{:,.0f}'.format(x)
    else:  # 소수점 아래 값이 0이 아닌 경우
        return '{:,.2f}'.format(x)


pd.options.display.float_format = custom_float_format


def inf_to_nan(sr):
    return sr.replace([np.inf, -np.inf], np.nan)


class MyAssets:

    def __init__(self, base_dt=None):

        if base_dt:
            self.today = pd.to_datetime(base_dt)
            self.year = self.today.year
        else:
            self.today = pd.Timestamp.today().normalize()
            self.year = pd.Timestamp.today().year

        self.assets = pd.read_excel('trade.xlsx', sheet_name='자산정보')
        self.assets['종목코드'] = self.assets['종목코드'].astype('str')
        self.ex_usd = get_exchange_rate('달러')
        self.ex_jpy = get_exchange_rate('엔')/100
        self.daily_trading = None
        self.bs_pl_book = None
        self.bs_pl_mkt = None
        self.bl = None
        self.my = None
        self.allo0 = None
        self.allo1 = None
        self.allo2 = None
        self.allo3 = None
        self.allo4 = None
        self.allo5 = None
        self.plot_pie = None
        self.plot_pie2 = None
        self.plot_pie3 = None

        self.class_returns = None

    def get_daily_trading(self):
        """ trade 엑셀파일에 있는 모든 시트들을 모아서 종목별 일일 거래내역 DataFrame을 반환 """

        days = pd.date_range(start=f'{self.year}-01-01', end=f'{self.year}-12-31', freq='D')

        usd1 = pd.read_excel('trade.xlsx', sheet_name='불리오달러')
        usd2 = pd.read_excel('trade.xlsx', sheet_name='한투달러')
        jpy = pd.read_excel('trade.xlsx', sheet_name='한투엔화')
        krw1 = pd.read_excel('trade.xlsx', sheet_name='나무원화')
        krw2 = pd.read_excel('trade.xlsx', sheet_name='한투원화')
        krw3 = pd.read_excel('trade.xlsx', sheet_name='한투CMA')
        krw4 = pd.read_excel('trade.xlsx', sheet_name='한투ISA')
        krw5 = pd.read_excel('trade.xlsx', sheet_name='별도원화')
        fiw = pd.read_excel('trade.xlsx', sheet_name='외화자산평가').drop('외화입출금', axis=1).copy()

        as_list = pd.DataFrame(
            product(self.assets['종목코드'], days),
            columns=['종목코드', '거래일자'])

        trade_raw = pd.concat([usd1, usd2, jpy, krw1, krw2,
                              krw3, krw4, krw5, fiw]).drop(['종목명','상품명'],axis=1)
        trade_raw['종목코드'] = trade_raw['종목코드'].astype('str')
        
        trade = as_list \
            .merge(self.assets[['종목코드','종목명', '통화', '계좌']], on='종목코드', how='left') \
            .merge(trade_raw,
                   on=['종목코드', '거래일자'],
                   how='left').fillna(0) \
            .sort_values(['종목코드', '거래일자']) \
            .reset_index(drop=True)

        trade['순매입수량'] = trade.매입수량 - trade.매도수량
        trade['수익'] = trade.매매수익 + trade.이자배당액
        trade['비용'] = trade.매입비용 + trade.매도비용
        trade['실현손익'] = trade.수익 - trade.비용

        self.daily_trading = trade[['종목코드', '거래일자', '종목명', '통화','계좌', '순매입수량', '매입액',
                                    '매도원금', '수익', '비용', '실현손익', '현금수입', '입출금', '현금지출']]

    def get_bs_pl(self):

        if self.daily_trading is None:
            self.get_daily_trading()

        trade = self.daily_trading

        bs_pl = trade[['종목코드', '거래일자', '종목명', '통화','계좌']].copy()
        bs_pl['보유수량'] = trade.groupby('종목코드').apply(lambda x: x.순매입수량.cumsum()).reset_index(drop=True)
        bs_pl['장부금액'] = trade.groupby('종목코드').apply(lambda x: (x.매입액 - x.매도원금).cumsum()).reset_index(drop=True)
        bs_pl['평잔'] = bs_pl.groupby('종목코드')['장부금액'].expanding().mean().reset_index(drop=True)
        bs_pl_add = trade.groupby('종목코드', group_keys=False)[['수익','비용','실현손익']].apply(lambda x: x.cumsum()).reset_index(drop=True)
        bs_pl = pd.concat([bs_pl,bs_pl_add],axis=1).copy()



        cash_w = trade.loc[bs_pl.통화=='원화'].groupby(['거래일자','계좌']) \
            .apply(lambda x: (x.현금수입 + x.입출금 - x.현금지출).sum()) \
            .unstack(level=1)[['나무','한투','한투CMA','한투ISA']]

        cash_w_b = cash_w.cumsum()
        cash_w_e = cash_w_b.expanding().mean()

        cash_d = trade.loc[bs_pl.통화=='달러'].groupby(['거래일자','계좌']) \
            .apply(lambda x: (x.현금수입 + x.입출금 - x.현금지출).sum()) \
            .unstack(level=1)

        cash_d_b = cash_d.cumsum()
        cash_d_e = cash_d_b.expanding().mean()

        cash_y = trade.loc[bs_pl.통화=='엔화'].groupby(['거래일자','계좌']) \
            .apply(lambda x: (x.현금수입 + x.입출금 - x.현금지출).sum()) \
            .unstack(level=1)

        cash_y_b = cash_y.cumsum()
        cash_y_e = cash_y_b.expanding().mean()


        bs_pl.loc[bs_pl.종목명 == '나무예수금','장부금액'] = cash_w_b.나무.values
        bs_pl.loc[bs_pl.종목명 == '한투예수금','장부금액'] = cash_w_b.한투.values
        bs_pl.loc[bs_pl.종목명 == '한투CMA예수금','장부금액'] = cash_w_b.한투CMA.values
        bs_pl.loc[bs_pl.종목명 == '한투ISA예수금','장부금액'] = cash_w_b.한투ISA.values
        bs_pl.loc[bs_pl.종목명 == '나무예수금','평잔'] = cash_w_e.나무.values
        bs_pl.loc[bs_pl.종목명 == '한투예수금','평잔'] = cash_w_e.한투.values
        bs_pl.loc[bs_pl.종목명 == '한투CMA예수금','평잔'] = cash_w_e.한투CMA.values
        bs_pl.loc[bs_pl.종목명 == '한투ISA예수금','평잔'] = cash_w_e.한투ISA.values

        bs_pl.loc[bs_pl.종목명 == '불리오달러','장부금액'] = cash_d_b.불리오.values
        bs_pl.loc[bs_pl.종목명 == '직접운용달러','장부금액'] = cash_d_b.한투.values
        bs_pl.loc[bs_pl.종목명 == '불리오달러','평잔'] = cash_d_e.불리오.values
        bs_pl.loc[bs_pl.종목명 == '직접운용달러','평잔'] = cash_d_e.한투.values

        bs_pl.loc[bs_pl.종목명 == '직접운용엔','장부금액'] = cash_y_b.한투.values
        bs_pl.loc[bs_pl.종목명 == '직접운용엔','평잔'] = cash_y_e.한투.values

        bs_pl['실현수익률'] = inf_to_nan(bs_pl.실현손익 / bs_pl.평잔 * 100)
        bs_pl = bs_pl.merge(self.assets[['종목명', '자산군', '세부자산군', '세부자산군2']], on='종목명', how='left')


        self.bs_pl_book = bs_pl.sort_values(['종목명','거래일자']).reset_index(drop=True)

    def plot_fund_ts(self):

        if self.bs_pl_book is None:
            self.get_bs_pl()

        df = self.bs_pl_book
        df1 = df.loc[df.통화 == '원화', ['거래일자','장부금액']].set_index('거래일자').groupby('거래일자').sum().장부금액
        df2 = df.loc[df.자산군 == '현금성',['거래일자','통화','장부금액']].groupby(['통화','거래일자']).sum().reset_index()
        df2 = df2[df2.통화=='원화'].set_index('거래일자').장부금액
        df3 = pd.DataFrame(index=df1.index).assign(총투자자산 = df1, 현금성자산 = df2).loc[:pd.Timestamp.now()].copy()

        df4 = pd.read_excel('trade.xlsx', sheet_name='현금흐름').fillna(0)
        df4['총투자자산_예측'] = df4.원화자금유입 - df4.원화자금유출
        df4['현금성자산_예측'] = df4.원화자금유입 + df4.원화투자회수 - df4.원화투자지출 - df4.원화자금유출
        df4 = df4.set_index('거래일자').loc[pd.Timestamp.now():, ['총투자자산_예측', '현금성자산_예측']]
        df4.iloc[0] = df4.iloc[0].values + df3.iloc[-1].values
        df5 = df4.cumsum()

        df6 = pd.concat([df3,df5],axis=1).copy()
        fig = px.line(df6)
        fig.update_layout(title='총투자자산 및 현금성자산 추이', xaxis_title='', yaxis_title='', title_x=0.5)
        fig.show()


    def evaluate_bs_pl(self):

        if (self.bl is None) and (self.my is None):
            self.bl = AutoInvest('boolio')
            self.my = AutoInvest('my')

        price = self.assets.loc[self.assets.평가금액.notnull()][['종목명','평가금액']]
        price = pd.concat(
            [price, self.my.inquire_balance(), self.my.inquire_balance_ovs(), 
             self.my.inquire_balance_ovs('JPY'), self.bl.inquire_balance_ovs()]
        )[['종목코드','평가금액']].astype({'평가금액':'float'})

        if self.bs_pl_book is None:
            self.get_bs_pl()

        bs_pl = self.bs_pl_book.query(f'거래일자 == "{self.today.strftime("""%Y-%m-%d""")}"')\
                    .merge(price, on='종목코드', how='left')
        bs_pl.loc[bs_pl.종목명 == '롯데케미칼', '평가금액'] = \
            int(self.bl.get_current_price("011170")['stck_prpr'])*70
        bs_pl['평가금액'] = bs_pl.평가금액.mask(bs_pl.평가금액.isnull(), bs_pl.장부금액)
        bs_pl.loc[bs_pl.종목명=='달러자산','평가금액'] = round(bs_pl.loc[bs_pl.통화=='달러'].평가금액.sum() * self.ex_usd, 0)
        bs_pl.loc[bs_pl.종목명=='엔화자산','평가금액'] = round(bs_pl.loc[bs_pl.통화=='엔화'].평가금액.sum() * self.ex_jpy, 0)

        bs_pl['평가손익'] = bs_pl.평가금액 - bs_pl.장부금액
        bs_pl['평가수익률'] = inf_to_nan(bs_pl.평가손익 / bs_pl.평잔 * 100)
        bs_pl['총손익'] = bs_pl.실현손익 + bs_pl.평가손익
        bs_pl['운용수익률'] = inf_to_nan(bs_pl.총손익 / bs_pl.평잔 * 100)

        self.bs_pl_mkt = bs_pl.sort_values(['통화','평가금액'], ascending=False).reset_index(drop=True)
        return self.bs_pl_mkt

    def compute_allocation(self):

        if self.bs_pl_mkt is None:
            self.evaluate_bs_pl()

        df = self.bs_pl_mkt.copy()
        df.loc[df.통화=='달러','평가금액'] = round(df.loc[df.통화=='달러','평가금액'] * self.ex_usd, 0)
        df.loc[df.통화=='엔화','평가금액'] = round(df.loc[df.통화=='엔화','평가금액'] * self.ex_jpy, 0)



        df= df.groupby(['자산군','세부자산군','통화'])[['평가금액']] \
            .sum().drop('외화자산') \
            .assign(
            투자비중 = lambda x: x.평가금액 / x.평가금액.sum() * 100
        )

        self.allo0 = df.reset_index(level=2, drop=True).groupby('자산군').sum()

        df1 = df.copy()
        df1.loc[('합계','',''),:] = [df1.평가금액.sum(),100]
        df1['투자비중(자산군별)'] = df1.groupby('자산군', group_keys=False)['평가금액'].apply(lambda x: x/x.sum()*100)
        self.allo1 =  df1.copy()


        df2 = df.reorder_levels([2,0,1]).reset_index(level=2, drop=True).sort_index(level=0).copy()
        df2 = df2.groupby(['통화','자산군']).sum().copy()
        df2.loc[('합계',''),:] = [df2.평가금액.sum(),100]
        df2['투자비중(통화별)'] = df2.groupby('통화', group_keys=False)['평가금액'].apply(lambda x: x/x.sum()*100)
        self.allo2 = df2.copy()

        self.allo3 = df.groupby('통화').sum()

        df3 = self.bs_pl_mkt
        df3 = df3.loc[df3.계좌=='불리오',['세부자산군','세부자산군2','평가금액']].groupby(['세부자산군','세부자산군2'])[['평가금액']] \
            .sum() \
            .assign(
            투자비중 = lambda x: x.평가금액 / x.평가금액.sum() * 100
        ).copy()
        self.allo4 = df3

        self.allo5 = df3.groupby('세부자산군').sum()

    def plot_allocation(self):

        if self.allo1 is None :
            self.compute_allocation()

        self.plot_pie = px.sunburst(
            self.allo1.투자비중.iloc[:-1].reset_index(),
            path=['자산군','세부자산군'], values='투자비중',
            width=500, height=500)

        self.plot_pie2 = px.sunburst(
            self.allo4.투자비중.reset_index(),
            path=['세부자산군','세부자산군2'], values='투자비중',
            width=500, height=500)

        self.plot_pie3 = px.sunburst(
            self.allo2.투자비중.iloc[:-1].reset_index(),
            path=['통화','자산군'], values='투자비중',
            width=500, height=500)

    def get_class_returns(self):

        if self.bs_pl_mkt is None:
            self.evaluate_bs_pl()

        df = self.bs_pl_mkt.copy()
        krw = df.loc[df.통화 == '원화'].copy()
        usd = df.loc[df.통화 == '달러'].copy()
        jpy = df.loc[df.통화 == '엔화'].copy()

        def get_class_returns_cur(asset_returns):

            cur = asset_returns.통화.iloc[0]
            class_returns = asset_returns \
                .drop(['종목명', '통화', '실현수익률', '평가수익률', '운용수익률'], axis=1) \
                .groupby(['자산군', '세부자산군']).sum(numeric_only=True)

            class_returns.loc[(f'{cur}전체',''), :] = class_returns.sum(numeric_only=True).values
            class_returns['실현수익률'] = inf_to_nan(class_returns.실현손익 / class_returns.평잔 * 100)
            class_returns['평가수익률'] = inf_to_nan(class_returns.평가손익 / class_returns.평잔 * 100)
            class_returns['운용수익률'] = inf_to_nan(class_returns.총손익 / class_returns.평잔 * 100)

            return class_returns

        krw_ret = get_class_returns_cur(krw)
        usd_ret = get_class_returns_cur(usd)
        jpy_ret = get_class_returns_cur(jpy)

        self.class_returns = pd.concat([krw_ret,usd_ret, jpy_ret])[
            ['장부금액','평가금액','실현손익','평가손익','실현수익률','평가수익률','운용수익률']
        ]

        return self.class_returns



def get_days():

    today = pd.Timestamp.today().normalize() - pd.DateOffset(days=1)
    days = namedtuple('days', ['m1', 'y1', 'y3', 'y10', 'y30', 'today'])
    res = days(m1 = (today - pd.DateOffset(months=1)).strftime('%Y%m%d'),
               y1 = (today - pd.DateOffset(years=1)).strftime('%Y%m%d'),
               y3 = (today - pd.DateOffset(years=3)).strftime('%Y%m%d'),
               y10 = (today - pd.DateOffset(years=10)).strftime('%Y%m%d'),
               y30 = (today - pd.DateOffset(years=30)).strftime('%Y%m%d'),
               today = today.strftime('%Y%m%d'))

    return res


class EcosData():

    def __init__(self):
        self.service_key = get_config()['ecos']
        self.api = Ecos(self.service_key)
        self.ts = {}

    def search_codes(self, word=''):
        df = self.api.get_statistic_table_list()
        df = df.loc[df.통계명.str.contains(word)].fillna('')
        return df

    def search_stats(self, stat=None):
        df = self.api.get_statistic_item_list(stat).fillna('')
        return df

    def generate_ts(self, name):

        ecos = pd.read_excel('ecos.xlsx', dtype='str')
        info = ecos[ecos.통계항목명 == name].iloc[0]
        days = get_days()
        df = self.api.get_statistic_search(info.통계표코드, info.주기, days.y30, days.today, info.통계항목코드)
        df['값'] = pd.to_numeric(df.값)
        df['시점'] = pd.to_datetime(df.시점)
        self.ts[name] = df