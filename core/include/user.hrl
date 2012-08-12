-type channel_id()  :: integer().
-type object_id()   :: integer().
-type user_id()     :: integer().
-type facebook_id() :: integer().
-type url()         :: string().
-type time()        :: erlang:now().
-type timestamp()   :: integer().

-record(user, 
        {uid,
         username,
         password,
         %% after login
         ip,
         token
        }).

-record(user_info, 
        {uid           :: user_id(),
         fbid          :: facebook_id(),
         email         :: string(),
         avatar        :: url(),
         
         name          :: string(),
         surname       :: string(),
         age           :: integer() | undefined,
         sex           :: 'male' | 'female' | undefined,
         location      :: string(),
         register_date :: time()}).

-record(user_unread, {key :: {user_id(), timestamp()},
                      cid :: channel_id(),
                      oid :: object_id()
                     }).

-record(id_seq, {thing,
                 id = 1000000}).
                
