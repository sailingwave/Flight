library(XML)

flight_sum <- function(file,depart,arrive,append=F,outfile1="/Users/Nan/Desktop/temp.csv",
                       outfile2="/Users/Nan/Desktop/temp2.csv"){
    #outfile1: for all airlines
    #outfile2: for AirChina
    
    htmlfile = htmlTreeParse(file, error=function(...){},useInternalNodes = TRUE,)
    
    date = getNodeSet(htmlfile,"//meta[@property='og:url']")
    date = sapply(date,xmlAttrs)[2]
    date1 = gsub(paste0(".*",arrive,"/(.*)/.*"),"\\1",date)
    date2 = gsub(paste0(".*",arrive,"/.*/(.*)"),"\\1",date)
    
    #nodes for price
    nodes = getNodeSet(htmlfile,"//div[@class='flightresult resultrow ']//div[@class='maindatacell booking fpricecol']")
    price = sapply(nodes,xmlValue)
    price = gsub("\n+"," ",price)
    price = gsub(" +"," ",price)
    price = gsub("\\$","",price)
    price = sapply(price,function(x) strsplit(x,split = ' ')[[1]][2])
    names(price) = NULL
    
    nodes = getNodeSet(htmlfile,"//div[@class='flightresult resultrow ']//div[@class='rightCol tripdetailholder']")
    detail = sapply(nodes,function(x) xmlValue(x))
    detail = gsub("\n+"," ",detail)
    detail = gsub(" +"," ",detail)
    
    company = gsub(paste0(" (.*?) ",depart," .*"),"\\1",detail)
    
    trip = gsub(paste0(".*? (",depart,".*)"),"\\1",detail)

    dep1 = gsub(paste0(depart," ([0-9:ap]*) →.*"),"\\1",trip)    #departure time
    arr1 = gsub(paste0(".*→ ",arrive," ([0-9:ap]*).*"),"\\1",trip)    #arrival time
    dur1 = gsub(paste0(".*→ ",arrive," [0-9:ap]* ([0-9h ]*m) .*"),"\\1",trip)    #duration
    stop1 = gsub(paste0(".*→ ",arrive," [0-9:ap]* [0-9h ]*m ([12nstop ]*) .*"),"\\1",trip)    #stop
        
    dep2 = gsub(paste0(".*",arrive," ([0-9:ap]*) →.*"),"\\1",trip)    #departure time
    arr2 = gsub(paste0(".*→ ",depart," ([0-9:ap]*).*"),"\\1",trip)    #arrival time
    dur2 = gsub(paste0(".*→ ",depart," [0-9:ap]* ([0-9h ]*m) .*"),"\\1",trip)    #duration
    stop2 = gsub(paste0(".*→ ",depart," [0-9:ap]* [0-9h ]*m ([12nstop ]*) .*"),"\\1",trip)    #stop
    
    dat = data.frame(company=company,price=price,date_dep=date1,date_arr=date2,
                     departure1=dep1,arrival1=arr1,duration1=dur1,stop1=stop1,
                     departure2=dep2,arrival2=arr2,duration2=dur2,stop2=stop2)
    
    write.table(dat,outfile1,row.names=F,quote=F,sep=",",append=append,col.names=!append)
    
    # AirChina
    price_airchina = getNodeSet(htmlfile,"//a[@id='fs_airlines_CA_price']")
    price_airchina = sapply(price_airchina,xmlValue)
    dat2 = data.frame(date_dep=date1,date_arr=date2,price=price_airchina)
    
    write.table(dat2,outfile2,row.names=F,quote=F,sep=",",append=append,col.names=!append)
}
