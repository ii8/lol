### how to set up
```
cp config/template-settings.yml config/settings.yml
vim config/settings.yml
```
set ur stuffs
```SQL
create database lol;
#run the app so it creates tables
insert into `deployment` set `name` = "test", `domain` = "localhost";
insert into `page` set `deployment` = (select `id` from `deployment` where `domain` = "localhost"), `name` = "home", `template` = "default";
```
