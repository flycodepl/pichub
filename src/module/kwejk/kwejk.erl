%%% @author convict <convict@magda>
%%% @copyright (C) 2012, convict
%%% @doc
%%% Modul odpowiadajacy za obsluge portalu kwejk.pl
%%% @end
%%% Created : 14 Mar 2012 by convict <convict@magda>

-module(kwejk).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([first_init/0,
         check_update/0]).

-define(SERVER, ?MODULE).

-include("config.hrl").
-include("user.hrl").
-include("obj.hrl").

-record(state, {latest_id}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

check_update() ->
    ?SERVER ! check_update.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    %% LOAD CONFIG
    ?DEBUG("Loading config...", []),
    {ok, Conf} = get_config(),
    {ok, #state{latest_id = Conf#obj_config.latest_id}}.

first_init() ->
    ?DEBUG("Initial module", []),
    ?DB:create_table(obj_kwejk),
    ?DB:add_record(#id_seq{thing = obj_kwejk}),
    set_config().

set_config() ->
    ?DEBUG("Set config", []),
    R = #obj_config{oid = ?KWEJK_ID,
                    name = "kwejk.pl",
                    url = ?KWEJK_MAIN_PAGE,
                    latest_id = <<"1047391">>,
                    module_name = kwejk,
                    enabled = true},
    ?DB:add_record(R).

get_config() ->    
    ?DB:get_record(obj_config, ?KWEJK_ID).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({update_latest_id, LatestId}, State) ->
    NewState = State#state{latest_id = LatestId},
    {ok, Conf} = get_config(),
    ?DB:add_record(Conf#obj_config{latest_id = LatestId}),
    ?DEBUG("Update latest id: ~p", [LatestId]),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(check_update, State) ->
    LatestId  = State#state.latest_id,
    ?DEBUG("Check update...", []),
    case get_new_data(LatestId) of
        {ok, current} ->
            ?DEBUG("Nothing new", []),
            ok;
        {ok, NewItems} ->
            ?DEBUG("New '~b' items", [length(NewItems)]),
            {ok, UpdateLatestId} = add_new_items(NewItems),
            gen_server:cast(?MODULE, {update_latest_id, UpdateLatestId})
            %% updater:brodcast(update, ?KWEJK_ID)
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%% add_new_items([F | Rest] = L) ->
%%     R = add_new_items(L, []),
%%     ?DB:add_records(R),
%%     {ok, F}.

%% add_new_items([F | Rest], Acc) ->
%%     [_Text, Id] = string:tokens(binary_to_list(F), "-"),
%%     ProvUrl = lists:flatten(?KWEJK_MAIN_PAGE++"/obrazek/"++Id),
%%     R = obj_kwejk(id = ?DB:get_next(obj_kwejk),
%%                   provider_url = ProvUrl,
%%                   provider_img = ProvImg

add_new_items([A | _Rest] = All) ->
    ?DB:add_records(All),
    {ok, A#obj_kwejk.id}.

get_new_data(LId) ->
    get_new_ids(get_site(), LId, []).

get_new_ids({NextPage, Data}, LId, Acc) ->
    MediaCollection = get_media_collection(Data),
    
    case parse_media(MediaCollection, LId) of
        {true, []} ->
            case Acc of 
                [] -> {ok, current};
                [_|_] -> Return = lists:reverse(Acc), {ok, lists:flatten(Return)}
            end;
        {true, Rec} -> Return = lists:reverse([Rec | Acc]), {ok, lists:flatten(Return)};
        {false, Rec} ->
            NewAcc = [Rec | Acc],
            get_new_ids(get_site(NextPage), LId, NewAcc)
    end.

parse_media(LMedia, LId) ->
    parse_media(LMedia, LId, []).

parse_media([], _LId, Acc) ->
    {false, Acc};
parse_media([M | Rest], LId, Acc) ->
    R = media_to_record(M),
    case R#obj_kwejk.id of
        LId -> {true, Acc};
        _Other -> 
            NewAcc = [R | Acc],
            parse_media(Rest, LId, NewAcc)
    end.

get_site() ->
    get_site(undefined).
get_site(Page) ->
    Url = case Page of
              undefined ->
                  ?KWEJK_MAIN_PAGE;
              _ ->
                  ?KWEJK_OTHER_PAGE++integer_to_list(Page)
          end,
    case utils:get_data_from_site(Url) of
        {ok, _Header, Data} ->
            io:fwrite("Page: ~p~n", [Page]),
            ParseData = mochiweb_html:parse(Data),
            NextPage = get_current_page(ParseData),
            {NextPage-1, ParseData};
        {error, _Desc} ->
            []
    end.


media_to_record(Media) ->
    Id = get_media_info(id, Media),
    ProvUrl = get_media_info(provider_url, Media),
    
    Obj = #obj_kwejk{id = Id,
                     provider_url = ProvUrl},
    case get_media_info(media_url, Media) of
        {true, MediaUrl} ->
            Obj#obj_kwejk{media_url = MediaUrl,
                          media_type = guess_media_type(MediaUrl)};
        {false, MediaContent} ->
            Obj#obj_kwejk{other_content = MediaContent,
                          media_type = {other, media_content}}
    end.
            

get_media_collection(Data) ->
    mochiweb_xpath:execute("//div[@class='shot']", Data).

get_current_page(Data) ->
    [Page] = mochiweb_xpath:execute("//span[@class='current page']/text()", Data),
    list_to_integer(binary_to_list(Page)).

get_media_info(id, Media) ->
    [FullId] = mochiweb_xpath:execute("//div[@class='shot']/@title", Media),
    [_, Id0] = string:tokens(binary_to_list(FullId), "-"),
    list_to_binary(Id0);
get_media_info(provider_url, Media) ->
    [ProvUrl] = mochiweb_xpath:execute("//a[@name='fb_share']/@share_url", Media),
    ProvUrl;
get_media_info(media_url, Media) ->
    case mochiweb_xpath:execute("//*[substring(@src,1,4) = 'http']/@src", Media) of
        [MediaUrl] -> {true, MediaUrl};
        X when is_list(X) -> {false, get_media_info(media_content, Media)}
    end;
get_media_info(media_content, Media) ->
    [MediaContent] = mochiweb_xpath:execute("//div[@class='media']", Media),
    MediaContent.




guess_media_type({_P, "www.youtube.com", _, _, _}) -> {video, youtube};
guess_media_type({_P, "youtube.com", _, _, _}) -> {video, youtube};
guess_media_type({_P, "player.vimeo.com", _, _, _}) -> {video, vimeo};
guess_media_type({_P, _Site, RestPath, _, _}) ->
    case string:right(RestPath, 4) of
        ".gif" -> {image, gif};
        ".jpg" -> {image, jpeg};
        "jpeg" -> {image, jpeg};
        ".png" -> {image, png};
        ".bmp" -> {image, bmp};
        ".swf" -> {application, 'x-shockwave-flash'};
        _ -> {data, corrupted}
    end;

guess_media_type(Url) ->
    guess_media_type(mochiweb_util:urlsplit(binary_to_list(Url))).



%% http://i1.kwejk.pl/site_media/obrazki/2012/03/ca681ab3c22c281e8b50b014298ea86b.gif?1332245106
