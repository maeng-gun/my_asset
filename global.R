# =============================================================================
# global.R — 앱 전역 설정 (패키지 로드, 소스 관리, 환경 감지)
# =============================================================================
# Shiny 앱 시작 시 최초 1회 실행
# =============================================================================

# --- 1. 패키지 로드 ---
library(shiny)
library(bs4Dash)
library(waiter)
library(flextable)
library(shinyWidgets)
library(lubridate)
library(shiny.pwa)
import::from(shinyjs, useShinyjs, extendShinyjs, js)

library(data.table)
library(tidyverse)
library(timetk)
library(R6)
library(glue)
library(httr)
library(jsonlite)
library(rvest)
library(RPostgres)
library(dbx)
library(tidyquant)
library(pool)
library(scales)
library(PerformanceAnalytics)


# --- 2. 환경 감지 ---
# SHINY_PORT가 비어있으면 로컬 환경, 값이 있으면 shinyapps.io 배포 환경
IS_LOCAL <- nchar(Sys.getenv("SHINY_PORT")) == 0


# --- 3. src/ 디렉토리 소싱 (도메인 로직) ---
# Shiny의 R/ 폴더 자동 소싱(알파벳 순)으로 인한 클래스 상속 및 패키지 로드 오류 방지
r_files <- c(
  "src/class_my_data.R",
  "src/class_auto_invest.R",
  "src/class_my_assets.R",
  "src/utils_flextable.R"
)
purrr::walk(r_files, source, encoding = "UTF-8")


# --- 4. modules/ 디렉토리 소싱 (Shiny 모듈) ---
module_files <- list.files("modules", pattern = "\\.R$", full.names = TRUE)
purrr::walk(module_files, source, encoding = "UTF-8")
