library(RMySQL)
library(dplyr)

# http://dev.mysql.com/doc/index-other.html
# $ mysql -u root -p mysql < world.sql

# mysql> desc Country;
# +----------------+--------------------------------------+------+-----+---------+-------+
# | Field          | Type                                 | Null | Key | Default | Extra |
# +----------------+--------------------------------------+------+-----+---------+-------+
# | Code           | char(3)                              | NO   | PRI |         |       |
# | Name           | char(52)                             | NO   |     |         |       |
# | Continent      | enum('Asia','Europe',...             | NO   |     | Asia    |       |
# | Region         | char(26)                             | NO   |     |         |       |
# | SurfaceArea    | float(10,2)                          | NO   |     | 0.00    |       |
# | IndepYear      | smallint(6)                          | YES  |     | NULL    |       |
# | Population     | int(11)                              | NO   |     | 0       |       |
# | LifeExpectancy | float(3,1)                           | YES  |     | NULL    |       |
# | GNP            | float(10,2)                          | YES  |     | NULL    |       |
# | GNPOld         | float(10,2)                          | YES  |     | NULL    |       |
# | LocalName      | char(45)                             | NO   |     |         |       |
# | GovernmentForm | char(45)                             | NO   |     |         |       |
# | HeadOfState    | char(60)                             | YES  |     | NULL    |       |
# | Capital        | int(11)                              | YES  |     | NULL    |       |
# | Code2          | char(2)                              | NO   |     |         |       |
# +----------------+--------------------------------------+------+-----+---------+-------+
# 15 rows in set (0.04 sec)

con <- dbConnect(MySQL(), host="localhost", port=25025, dbname="world",
                 user="root", password="your-password")

country <- dbGetQuery(con, "SET NAMES utf8")

# db has {City, Country, CountryLanguage} table
country <- dbGetQuery(con, "SELECT * FROM Country")

japan <- country %>% filter(Name == "Japan")
Hmisc::list.tree(japan)
# japan = list 15 (2752 bytes)( data.frame )
# .  Code = character 1= JPN
# .  Name = character 1= Japan
# .  Continent = character 1= Asia
# .  Region = character 1= Eastern Asia
# .  SurfaceArea = double 1= 377829
# .  IndepYear = integer 1= -660
# .  Population = integer 1= 126714000
# .  LifeExpectancy = double 1= 80.7
# .  GNP = double 1= 3787042
# .  GNPOld = double 1= 4192638
# .  LocalName = character 1= Nihon/Nippon
# .  GovernmentForm = character 1= Constitutional Monarch
# .  ...   and 3 more
# A  row.names = integer 1= 1
