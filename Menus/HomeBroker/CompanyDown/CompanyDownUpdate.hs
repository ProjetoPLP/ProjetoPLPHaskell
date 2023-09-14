module Menus.HomeBroker.CompanyDown.CompanyDownUpdate where

import Utils.UpdateUtils (resetMenu)
import Clock.ClockUpdate (updateMatrixClock)
import Menus.HomeBroker.HomeBrokerUpdate (updateHBCash, updateHBCompanyName, updateHBCompanyCode)
import Client.GetSetAttrsClient (getCash)
import Company.GetSetAttrsCompany (getName, getCode, getPrice)
import Company.SaveCompany (removeCompany)
import Client.PostClient (removeAllClientsAsset)


-- Atualiza todas as informações do menu de falência de uma empresa
updateCompanyDown :: Int -> Int -> IO ()
updateCompanyDown idUser idComp = do
    resetMenu filePath "./Sprites/HomeBroker/companyDown_base.txt"
    updateMatrixClock filePath
    updateHBCash filePath (getCash idUser)
    updateHBCompanyName filePath (getName idComp)
    updateHBCompanyCode filePath (getCode idComp)
    removeComapanyFromExchange idComp

    where filePath = "./Menus/HomeBroker/CompanyDown/companyDown.txt"


-- Remove completamente os registros de uma empresa na bolsa
removeComapanyFromExchange :: Int -> IO ()
removeComapanyFromExchange idComp = do
    removeCompany idComp "./Data/Companies.json"
    removeAllClientsAsset idComp


-- Verifica se uma empresa entrou em falência
isDown :: Int -> Bool
isDown idComp = getPrice idComp <= 0