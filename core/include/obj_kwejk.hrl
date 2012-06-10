-define(KWEJK_ID, 0).
-define(KWEJK_MAIN_PAGE, "http://kwejk.pl").
-define(KWEJK_OTHER_PAGE, ?KWEJK_MAIN_PAGE++"/strona/").

-record(obj_kwejk, 
        {id,
         provider_url = "" :: string(),
         media_url = ""    :: string(),
         media_type        :: ({video,youtube} | {image, jpeg} | {image, gif}),
         title = ""        :: string(),
         other_content = "":: string(),
         add_date = utils:time()
        }).
