-record(user, 
        {uid,
         username,
         password,
         %% after login
         ip,
         token
        }).

-record(user_info, 
        {uid,
         fbid,
         email,
         avatar,
         
         name = ""    :: string(),
         surname = "" :: string(),
         age = 0      :: integer(),
         sex          :: 'male' | 'female' | undefined,
         location     :: string(),
         register_date}).

-record(user_unread, {uid, oid}). %% oid - Object ID

-record(id_seq, {thing,
                 id = 1000000}).
                
