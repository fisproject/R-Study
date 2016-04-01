require(dplyr)
require(purrr)

# nested list
attractions <- list(
  list(name = 'カリブの海賊', time_required = 15, tags = c('アドベンチャーランド', 'ファストパス')),
  list(name = 'ビッグサンダー・マウンテン', time_required = 4, tags = c('ウエスタンランド', 'ファストパス', '人気')),
  list(name = 'スプラッシュ・マウンテン', time_required = 10, tags = c('クリッターカントリー', 'ファストパス', '人気'))
)

Hmisc::list.tree(attractions)
#  attractions = list 3 (2560 bytes)
# .  [[1]] = list 3
# . .  name = character 1= カリブの海賊
# . .  time_required = double 1= 15
# . .  tags = character 2= アドベンチャーランド ファストパス
# .  [[2]] = list 3
# . .  name = character 1= ビッグサンダー・マウンテン
# . .  time_required = double 1= 4
# . .  tags = character 3= ウエスタンランド ファストパス 人気
# .  [[3]] = list 3
# . .  name = character 1= スプラッシュ・マウンテン
# . .  time_required = double 1= 10
# . .  tags = character 3= クリッターカントリー ファストパス 人気

attractions %>% map('name')
# [[1]]
# [1] "カリブの海賊"
#
# [[2]]
# [1] "ビッグサンダー・マウンテン"
#
# [[3]]
# [1] "スプラッシュ・マウンテン"

attractions %>% map(~ .$time_required + 5)
# [[1]]
# [1] 20
#
# [[2]]
# [1] 9
#
# [[3]]
# [1] 15

new_list <- attractions %>%
    map(~ list(name = .$name, park = 'ディズニーランド', popular = '人気' %in% .$tags))
Hmisc::list.tree(new_list)
# new_list = list 3 (2168 bytes)
# .  [[1]] = list 3
# . .  name = character 1= カリブの海賊
# . .  park = character 1= ディズニーランド
# . .  popular = logical 1= FALSE
# .  [[2]] = list 3
# . .  name = character 1= ビッグサンダー・マウンテン
# . .  park = character 1= ディズニーランド
# . .  popular = logical 1= TRUE
# .  [[3]] = list 3
# . .  name = character 1= スプラッシュ・マウンテン
# . .  park = character 1= ディズニーランド
# . .  popular = logical 1= TRUE

over_ten_min <- attractions %>% keep(~ .$time_required >= 10)
Hmisc::list.tree(over_ten_min)
# over_ten_min = list 2 (1688 bytes)
# .  [[1]] = list 3
# . .  name = character 1= カリブの海賊
# . .  time_required = double 1= 15
# . .  tags = character 2= アドベンチャーランド ファストパス
# .  [[2]] = list 3
# . .  name = character 1= スプラッシュ・マウンテン
# . .  time_required = double 1= 10
# . .  tags = character 3= クリッターカントリー ファストパス 人気
