### how to set up
```
cp config/template-settings.yml config/settings.yml
vim config/settings.yml
```
set ur stuffs
```SQL
create database lol;
#run the app so it creates tables
insert into `deployment` set `name` = "test", `domain` = "localhost", `wrapper` = "navbar";
insert into `piece` set `name` = "test";
insert into `page` set `deployment` = (select `id` from `deployment` where `domain` = "localhost"), `name` = "home", `piece` = 1;
insert into `piece_data` set `piece` = 1, `key` = "mainbox", `value` = "Hello I am custom page data", `type` = "Plain";
```

pieces are widgets but I did not want to call them widget because thats in use by yesod already.
wrappers is what gets added to a page when using the defaultLayout, its specified per deployment and will usually contain a navbar.
