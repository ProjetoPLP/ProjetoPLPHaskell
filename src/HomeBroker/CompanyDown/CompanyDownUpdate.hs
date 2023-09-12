module HomeBroker.CompanyDown.CompanyDownUpdate where

import Utils.UpdateUtils (resetMenu)
import Clock.ClockUpdate (updateMatrixClock)
import HomeBroker.HomeBrokerUpdate (updateHBCash, updateHBCompanyName, updateHBCompanyCode)
import Client.GetSetAttrsClient (getCash)
import Company.GetSetAttrsCompany (getName, getCode, getPrice)
import Company.SaveCompany (removeCompany)
import Client.PostClient (removeAllClientsAsset)


updateCompanyDown :: Int -> Int -> IO ()
updateCompanyDown idUser idComp = do
    resetMenu filePath "./Sprites/HomeBroker/companyDown_base.txt"
    updateMatrixClock filePath
    updateHBCash filePath (getCash idUser)
    updateHBCompanyName filePath (getName idComp)
    updateHBCompanyCode filePath (getCode idComp)

    removeCompany idComp "./Data/Companies.json"
    removeAllClientsAsset idComp

    where filePath = "./HomeBroker/CompanyDown/companyDown.txt"


isDown :: Int -> Bool
isDown idComp = getPrice idComp <= 0