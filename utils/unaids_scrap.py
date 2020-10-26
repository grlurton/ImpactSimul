import subprocess
import sys
import time
import os
from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import TimeoutException
from selenium.webdriver.common.keys import Keys
import pandas as pd

def parse_results(browser):
    table_body = browser.find_element_by_xpath("//div/div[2]/div[2]/div/div[2]/div[5]/div/div/div[2]/div[2]/div/div/div/table/tbody")
    table_body1 = browser.find_element_by_xpath("//div/div[2]/div[2]/div/div[2]/div[5]/div/div/div[2]/div[2]/div/div[1]/div/table/tbody")
    table_body12 = browser.find_element_by_xpath("//div/div[2]/div[2]/div/div[2]/div[5]/div/div/div[2]/div[2]/div/div[12]/div/table/tbody")
    table_body3 = browser.find_element_by_xpath("//div/div[2]/div[2]/div/div[2]/div[5]/div/div/div[2]/div[2]/div/div[3]/div/table/tbody")
    table_body4 = browser.find_element_by_xpath("//div/div[2]/div[2]/div/div[2]/div[5]/div/div/div[2]/div[2]/div/div[4]/div/table/tbody")
    table_body6 = browser.find_element_by_xpath("//div/div[2]/div[2]/div/div[2]/div[5]/div/div/div[2]/div[2]/div/div[6]/div/table/tbody")
    table_body7 = browser.find_element_by_xpath("//div/div[2]/div[2]/div/div[2]/div[5]/div/div/div[2]/div[2]/div/div[7]/div/table/tbody")
    table_body8 = browser.find_element_by_xpath("//div/div[2]/div[2]/div/div[2]/div[5]/div/div/div[2]/div[2]/div/div[8]/div/table/tbody")

    results = []
    for row in table_body1.find_elements_by_xpath('./tr'):
        results.append(row.text)
    for row in table_body12.find_elements_by_xpath('./tr'):
        results.append(row.text)
    for row in table_body3.find_elements_by_xpath('./tr'):
        results.append(row.text)
    for row in table_body4.find_elements_by_xpath('./tr'):
        results.append(row.text)
    for row in table_body6.find_elements_by_xpath('./tr'):
        results.append(row.text)
    for row in table_body7.find_elements_by_xpath('./tr'):
        results.append(row.text)
    for row in table_body8.find_elements_by_xpath('./tr'):
        results.append(row.text)

    return results

def write_results(results, filename):
    """outputs results to .csv """

    df = pd.DataFrame(data={"Estimates": results})
    df.to_csv( filename + '.csv', sep=',',index=False)

def query_country(browser, country):
    """ queries a given country, passed as a string, to scrap UNAIDS' most recent HIV and AIDS estimates"""

    # pause briefly before proceding
    time.sleep(3.0)

    try:
        WebDriverWait(browser, 3).until(EC.alert_is_present(),
            'Timed out waiting for PA creation ' +
            'confirmation popup to appear.')

        alert = browser.switch_to_alert()
        alert.accept()
        print("alert accepted")

    except TimeoutException:
        print("no alert")

    factsheet = browser.find_elements_by_xpath("//a[contains(text(), 'Factsheets')]")
    #browser.execute_script("arguments[0].click();", factsheet)
    factsheet[0].click() 

    # pause briefly before proceding
    time.sleep(3.0)

    cntry= browser.find_elements_by_xpath("//a[contains(text(), 'Country')]")
    cntry[0].click()
    # pause briefly before proceding
    time.sleep(3.0)

    cntry_est = browser.find_elements_by_xpath("//a[contains(text(), '" + country + "')]")
    cntry_est[0].click()
    # pause briefly before proceding
    time.sleep(3.0)

    btn = browser.find_elements_by_id("btnViewqlData")
    btn[0].click()

    results = parse_results(browser)

    return results

def extract_unaids_py(country, url, path_to_chromedriver, outpath):
    browser = webdriver.Chrome(executable_path = path_to_chromedriver) # creates a chrome session
    
    print("getting url")
    browser.get(url) # Load page

    results = query_country(browser, str(country))

    write_results(results, "{}unaids_scrape.csv".format(outpath))

    browser.close()
