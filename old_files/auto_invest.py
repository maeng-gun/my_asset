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
import auto_invest as ai
from itables import init_notebook_mode


def get_config():

    with open(r'../config.yaml', encoding='UTF-8') as f:
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

            print('expire dt: ', exp_dt, ' vs now dt:', now_dt)
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
        print("saved_token: ", saved_token)

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

    def inquire_balance_ovs(self):

        path = "uapi/overseas-stock/v1/trading/inquire-balance"
        url = f"{self.URL_BASE}/{path}"
        data = {
            "CANO": self.ACCT,
            "ACNT_PRDT_CD": "01",
            "OVRS_EXCG_CD": "NASD",
            "TR_CRCY_CD": "USD",
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






init_notebook_mode(all_interactive=True)

def get_exchange_rate():

    res = requests.get("http://finance.naver.com/marketindex/").content
    soup = BeautifulSoup(res, "html.parser")

    return float(soup.select_one("div.head_info > span.value").string.replace(',',''))

def custom_float_format(x):
    if x == int(x):  # 실제로 소수점 아래 값이 0인 경우
        return '{:,.0f}'.format(x)
    else:  # 소수점 아래 값이 0이 아닌 경우
        return '{:,.2f}'.format(x)

pd.options.display.float_format = custom_float_format

def inf_to_nan(sr):
    return sr.replace([np.inf, -np.inf], np.nan)

def get_daily_trading(base_dt=None):
    """ trade 엑셀파일에 있는 모든 시트들을 모아서 종목별 일일 거래내역 DataFrame을 반환 """

    assets = pd.read_excel('trade.xlsx',sheet_name=0)

    if base_dt:
        year = base_dt[:4]
    else:
        year = pd.Timestamp.today().year

    days = pd.date_range(start=f'{year}-01-01', end=f'{year}-12-31', freq='D')

    usd1 = pd.read_excel('trade.xlsx',sheet_name=1)
    usd2 = pd.read_excel('trade.xlsx',sheet_name=2)
    fi   = pd.read_excel('trade.xlsx',sheet_name=3)
    krw1 = pd.read_excel('trade.xlsx',sheet_name=4)
    krw2 = pd.read_excel('trade.xlsx',sheet_name=5)
    krw3 = pd.read_excel('trade.xlsx',sheet_name=6)
    krw4 = fi.drop('달러입출금', axis=1).copy()

    as_list = pd.DataFrame(
        product(assets['종목코드'], days),
        columns=['종목코드', '거래일자'])

    trade = as_list \
        .merge(
        pd.concat([usd1, usd2, krw1, krw2, krw3, krw4]),
        on=['종목코드', '거래일자'],
        how='left'
    ).fillna(0) \
        .sort_values(['종목코드', '거래일자']) \
        .reset_index(drop=True)

    trade['보유수량'] = trade.groupby('종목코드').apply(lambda x: (x.매입수량 - x.매도수량).cumsum()).reset_index(drop=True)
    trade['장부금액'] = trade.groupby('종목코드').apply(lambda x: (x.매입액 - x.매도원금).cumsum()).reset_index(drop=True)
    trade['평잔'] = trade.groupby('종목코드')['장부금액'].expanding().mean().reset_index(drop=True)
    trade['수익'] = trade.매매수익 + trade.이자배당액
    trade['비용'] = trade.매입비용 + trade.매도비용
    trade['실현손익'] = trade.수익 - trade.비용

    return trade[['종목코드', '거래일자','종목명','보유수량', '장부금액',
                  '평잔', '수익','비용','실현손익', '현금수입', '입출금', '현금지출']]


def get_bs_pl(base_dt=None):

    assets = pd.read_excel('trade.xlsx',sheet_name=0)
    df_trade = get_daily_trading()

    if base_dt:
        today=pd.to_datetime(base_dt)
    else:
        today=pd.Timestamp.today().normalize()

    bs_pl = assets[['종목코드', '종목명', '통화']] \
        .merge(df_trade.loc[df_trade['거래일자'] == today, ['종목코드', '보유수량', '장부금액', '평잔']],
               on='종목코드', how='left') \
        .merge(df_trade[df_trade['거래일자'] <= today] \
               .groupby('종목코드').agg({
        '수익': 'sum',
        '비용': 'sum',
        '실현손익': 'sum'
    }).reset_index(),
               on='종목코드', how='left').copy()

    cash = df_trade \
        .merge(assets.loc[assets.통화=='달러',['종목코드','계좌']], on='종목코드', how='left') \
        .groupby(['거래일자','계좌']) \
        .apply(lambda x: (x.현금수입 + x.입출금 - x.현금지출).sum()) \
        .unstack(level=1)

    bs_pl.loc[bs_pl.종목명.str.contains('달러') & (bs_pl.통화=='달러'), '장부금액'] = \
        cash.cumsum().loc[today].values
    bs_pl.loc[bs_pl.종목명.str.contains('달러') & (bs_pl.통화=='달러'), '평잔'] = \
        cash.expanding().mean().loc[today].values

    bs_pl['실현수익률'] = inf_to_nan(bs_pl.실현손익 / bs_pl.평잔 * 100)

    return bs_pl.sort_values(['통화','장부금액'], ascending=False)

def evaluate_bs_pl():

    bl = ai.AutoInvest('boolio')
    my = ai.AutoInvest('my')
    assets = pd.read_excel('trade.xlsx',sheet_name=0)
    price = assets.loc[assets.평가금액.notnull()][['종목명','평가금액']]
    price = pd.concat(
        [price, my.inquire_balance(),bl.inquire_balance_ovs()]
    )[['종목코드','평가금액']].astype({'평가금액':'float'})
    bs_pl = get_bs_pl()
    bs_pl = bs_pl.merge(price, on='종목코드', how='left')
    bs_pl.loc[bs_pl.종목명 == '롯데케미칼', '평가금액'] = \
        int(bl.get_current_price("011170")['stck_prpr'])*70
    bs_pl['평가금액'] = bs_pl.평가금액.mask(bs_pl.평가금액.isnull(), bs_pl.장부금액)
    bs_pl.loc[bs_pl.종목명=='달러자산','평가금액'] = round(bs_pl.loc[bs_pl.통화=='달러'].평가금액.sum()*get_exchange_rate(),0)
    bs_pl['평가손익'] = bs_pl.평가금액 - bs_pl.장부금액
    bs_pl['평가수익률'] = inf_to_nan(bs_pl.평가손익 / bs_pl.평잔 * 100)
    bs_pl['총손익'] = bs_pl.실현손익 + bs_pl.평가손익
    bs_pl['운용수익률'] = inf_to_nan(bs_pl.총손익 / bs_pl.평잔 * 100)
    bs_pl = bs_pl.merge(assets[['종목명', '자산군', '세부자산군']], on='종목명', how='left')

    return bs_pl