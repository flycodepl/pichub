-include("obj_kwejk.hrl").

-record(obj_config,
        {oid,
         name = ""       :: string(),
         url = ""        :: string(),
         latest_id       :: string(),
         module_name       :: atom(),
         enabled = false :: boolean()
        }).
