

# load household raw data
household.df <- read.delim("data/householdData_2015.txt", header = FALSE, sep=",", na.strings = "nan")

# load the header easliy to manage the household data 
househead <- read.csv("data/df_header.csv", header = FALSE, sep=",", stringsAsFactors = FALSE )
hh_header <- as.list.data.frame(househead[1,])

# complete dataFrame of household (with Header)
colnames(household.df) <- hh_header 


# Individual Based
#    - each person/agent has properties

hh_agent_header <- c("id", "sex", "age", "edu", "nCars", "income", 
                     "marriage", "nFamily", "nKids", "job", "dummy")
HHagent.df <- data.frame(matrix(nrow = 0, ncol = length(hh_agent_header)))
colnames(HHagent.df) <- hh_agent_header

# Create a population of agents
cnt <- 1
agent_id <- 0

while(cnt < nrow(household.df)+1)
{
  # create a new dataframe for a household   
  tmp_agentData <- data.frame(matrix(nrow = household.df$hh_num[cnt], ncol = length(hh_agent_header)))
  colnames(tmp_agentData) <- hh_agent_header
  
  # save the info of household head 

  agent_id <- agent_id + 1
  
  tmp_agentData$id[1] <- agent_id
  tmp_agentData$sex[1] <- household.df$head_sex[cnt] - 1
  tmp_agentData$age[1] <- household.df$head_age[cnt]
  tmp_agentData$edu[1] <- ifelse(household.df$head_edu[cnt] < 3, 0, household.df$head_edu[cnt] - 2)
  tmp_agentData$nCars[1] <- household.df$cars_num[cnt] #/ household.df$hh_num[cnt]
  tmp_agentData$income[1] <- (round(household.df$hh_income[cnt], digits=-4 )/10000) %/% household.df$hh_num[cnt]
  tmp_agentData$nFamily[1] <- household.df$hh_num[cnt]
  tmp_agentData$job[1] <- ifelse(household.df$head_job == 1, 1, 0)
  
  # temporary variable for setting the social properties of an agent
  tmp_marriage <- 0
  tmp_nkids <- 0
  tmp_workchild <-0
  tmp_grandchild <-0
  
  # store the info of household members (microdata has the information of household member)     
  if (household.df$hh_num[cnt] > 1 )
  {
    for (j in seq(household.df$hh_num[cnt]-1))
    {
      if(household.df[cnt, names(household.df) == (paste0("hhm", j+1, "_info"))] == 2)
      {
        tmp_agentData$marriage[j+1] <- 1
        tmp_marriage <- 1
        
      }
      else 
      {
        # check kids   
        if(household.df[cnt, names(household.df) == (paste0("hhm", j+1, "_age"))] < 19 )
        {
          tmp_nkids <- tmp_nkids + 1
        }
        
        # check married children    
        if(household.df[cnt, names(household.df) == (paste0("hhm", j+1, "_info"))] == 4)
        {
          tmp_agentData$marriage[j+1] <- 1
        }
        else
        {
          tmp_agentData$marriage[j+1] <- 0
          
          if(household.df[cnt, names(household.df) == (paste0("hhm", j+1, "_info"))] == 5)
          {
            tmp_grandchild <- tmp_grandchild + 1
          }
        }
      }
    }
    
    
    for (tmp_cnt in seq(household.df$hh_num[cnt]-1))
    {
      agent_id <- agent_id + 1
      tmp_agentData$id[tmp_cnt+1] <- agent_id
      
      tmp_agentData$sex[tmp_cnt+1] <- household.df[cnt, names(household.df) == (paste0("hhm", tmp_cnt+1, "_sex"))] - 1
      tmp_agentData$age[tmp_cnt+1] <- household.df[cnt, names(household.df) == (paste0("hhm", tmp_cnt+1, "_age"))]
      tmp_agentData$edu[tmp_cnt+1] <- ifelse(household.df[cnt, names(household.df) == (paste0("hhm", tmp_cnt+1, "_edu"))] < 3, 
                                             0, household.df[cnt, names(household.df) == (paste0("hhm", tmp_cnt+1, "_edu")) ] -2)
      
      tmp_agentData$nCars[tmp_cnt+1] <- household.df$cars_num[cnt] #/ household.df$hh_num[cnt]
      tmp_agentData$income[tmp_cnt+1] <- (round(household.df$hh_income[cnt], digits=-4 )/10000) %/% household.df$hh_num[cnt]
      tmp_agentData$nFamily[tmp_cnt+1] <- household.df$hh_num[cnt]
      tmp_agentData$job[tmp_cnt+1] <- ifelse(household.df[cnt, names(household.df) == (paste0("hhm", tmp_cnt+1, "_job"))] == 1, 
                                             1, 0)
      
      tmp_agentData$dummy[tmp_cnt+1] <- 1
      
      if(household.df[cnt, names(household.df) == (paste0("hhm", tmp_cnt+1, "_info"))] == 2)
      {
        tmp_agentData$nKids[tmp_cnt+1] <- tmp_nkids
      }
      else
      {
        if(household.df[cnt, names(household.df) == (paste0("hhm", j+1, "_info"))] == 4)
        {
          tmp_agentData$nKids[tmp_cnt+1] <- tmp_grandchild
        }
        else
          tmp_agentData$nKids[tmp_cnt+1] <- 0
      }
    }
  }
  
  tmp_agentData$nKids[1] <- tmp_nkids
  tmp_agentData$marriage[1] <- tmp_marriage
  #  tmp_agentData$dummy[1] <- 1
  
  HHagent.df <- rbind(HHagent.df, tmp_agentData)  
  rm(tmp_agentData)
  
  cnt <- cnt + 1
}

# 
HHagent.df$edu[which(HHagent.df$edu==5)] <- 4
HHagent.df$job[is.na(HHagent.df$job)] <- 0
HHagent.df$dummy <-1


