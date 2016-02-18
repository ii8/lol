### how to set up
```
cp config/template-settings.yml config/settings.yml
vim config/settings.yml
```
set ur stuffs
```SQL
create database lol;
#run the app so it creates tables
insert into `deployment` set
    `name` = "test",
    `domain` = "localhost",
    `wrapper` = "navbar",
    `stripe_public` = "pk_test_wUAzZy675JJKqVCFMU2FcSBB",
    `stripe_secret` = "sk_test_cC82zVtq4YT0Y90ZuXesESTX",
    `email` = "dave@sharklasers.com";
insert into `piece` set
    `deployment` = 1,
    `template` = "test";
insert into `page` set
    `deployment` = 1,
    `name` = "home",
    `piece` = 1;
insert into `piece_data` set
    `piece` = 1,
    `key` = "mainbox",
    `value` = "Hello I am custom page data",
    `type` = "Plain";
```

pieces are widgets but I did not want to call them widget because thats in use by yesod already.
wrappers is what gets added to a page when using the defaultLayout, its specified per deployment and will usually contain a navbar.
