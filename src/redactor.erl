%%%-------------------------------------------------------------------
%%% @author redeye
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. июн 2014 16:02
%%%-------------------------------------------------------------------
-module(redactor).
-author("redeye").
-behaviour(wx_object).
-include_lib("wx/include/wx.hrl").


-export([start/0, start/1, start_link/0, start_link/1, format/3,
  init/1, terminate/2, code_change/3,
  handle_info/2, handle_call/3, handle_cast/2, handle_event/2, say/2]).


-record(state, {win, demo, example, selector, log, code, curfile}).

%% For wx-2.9 usage
-ifndef(wxSTC_ERLANG_COMMENT_FUNCTION).
-define(wxSTC_ERLANG_COMMENT_FUNCTION, 14).
-define(wxSTC_ERLANG_COMMENT_MODULE, 15).
-define(wxSTC_ERLANG_COMMENT_DOC, 16).
-define(wxSTC_ERLANG_COMMENT_DOC_MACRO, 17).
-define(wxSTC_ERLANG_ATOM_QUOTED, 18).
-define(wxSTC_ERLANG_MACRO_QUOTED, 19).
-define(wxSTC_ERLANG_RECORD_QUOTED, 20).
-define(wxSTC_ERLANG_NODE_NAME_QUOTED, 21).
-define(wxSTC_ERLANG_BIFS, 22).
-define(wxSTC_ERLANG_MODULES, 23).
-define(wxSTC_ERLANG_MODULES_ATT, 24).
-endif.

-define(stc, wxStyledTextCtrl).

start() ->
  start([]).

start(Debug) ->
  wx_object:start(?MODULE, Debug, []).

start_link() ->
  start([]).

start_link(Debug) ->
  wx_object:start_link(?MODULE, Debug, []).

format(Config, Str, Args) ->
  Log = proplists:get_value(log, Config),
  wxTextCtrl:appendText(Log, io_lib:format(Str, Args)),
  ok.

-define(LOADFILE, 101).
-define(SAVEFILE, 102).
-define(DEBUG_TRACE, 103).
-define(DEBUG_DRIVER, 104).

init(Options) ->
  wx:new(Options),
  process_flag(trap_exit, true),
  Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Erlang IDE", [{size, {1000, 500}}]),
  MB = wxMenuBar:new(),
  File = wxMenu:new([]),
  wxMenu:append(File, ?LOADFILE, "&Load file"),
  wxMenu:appendSeparator(File),
  wxMenu:append(File, ?SAVEFILE, "&Save file"),
  wxMenu:appendSeparator(File),
  wxMenu:append(File, ?wxID_PRINT, "&Print code"),
  wxMenu:appendSeparator(File),
  wxMenu:append(File, ?wxID_EXIT, "&Quit"),
  Help = wxMenu:new([]),
  wxMenu:append(Help, ?wxID_HELP, "Help"),
  wxMenu:append(Help, ?wxID_ABOUT, "About"),
  wxMenuBar:append(MB, File, "&File"),
  wxMenuBar:append(MB, Help, "&Help"),
  wxFrame:setMenuBar(Frame, MB),

  wxFrame:connect(Frame, command_menu_selected),
  wxFrame:connect(Frame, close_window),
  _SB = wxFrame:createStatusBar(Frame, []),
  Code = code_area(Frame),
  wxFrame:show(Frame),
%%   load_code(Code, {ok, <<"-module(erlide_app).\n-author(\"redeye\").\n-behaviour(application).\n%% Application callbacks\n-export([start/2,stop/1]).">>}),
  State = #state{win = Frame, code = Code},
  io:format("5 ~n"),
  {Frame, State}.

%%%%%%%%%%%%
%% Callbacks

%% Handled as in normal gen_server callbacks
handle_info({'EXIT', _, wx_deleted}, State) ->
  {noreply, State};
handle_info({'EXIT', _, shutdown}, State) ->
  {noreply, State};
handle_info({'EXIT', _, normal}, State) ->
  {noreply, State};
handle_info(Msg, State) ->
  io:format("Got Info ~p~n", [Msg]),
  {noreply, State}.

handle_call(Msg, _From, State) ->
  io:format("Got Call ~p~n", [Msg]),
  {reply, ok, State}.

handle_cast(Msg, State) ->
  io:format("Got cast ~p~n", [Msg]),
  {noreply, State}.

%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{id = Id,
  event = #wxCommand{type = command_menu_selected}},
    State = #state{win = Panel, code = Code, curfile = FileName}) ->
  case Id of
    ?SAVEFILE ->
      case FileName of
        undefined -> {noreply, State};
        NotEmpty ->
          TEXT = wxStyledTextCtrl:getText(Code),
        % say(Panel,TEXT),
        %  say(Panel,NotEmpty),
          file:write_file(NotEmpty, unicode:characters_to_binary(TEXT)),
          {noreply, State}
      end;
    ?LOADFILE ->
      Dialog = apply(wxFileDialog, new, [Panel, []]),
      case wxFileDialog:showModal(Dialog) of
        ?wxID_OK ->
          FName = wxFileDialog:getPath(Dialog),
          load_code(Code, file:read_file(FName)),
          wxFileDialog:destroy(Dialog),
          {noreply, State#state{code = Code, curfile = FName}};
        _Any ->
          wxFileDialog:destroy(Dialog),
          {noreply, State}
      end;
    ?wxID_PRINT ->
%% If you are going to printout mainly text it is easier if
%% you generate HTML code and use a wxHtmlEasyPrint
%% instead of using DCs
      Module = "ex_" ++ wxListBox:getStringSelection(State#state.selector) ++ ".erl",
      HEP = wxHtmlEasyPrinting:new([{name, "Print"},
        {parentWindow, State#state.win}]),
      Html = demo_html_tagger:erl2htmltext(Module),
      wxHtmlEasyPrinting:previewText(HEP, Html),
      {noreply, State};
    ?wxID_HELP ->
      wx_misc:launchDefaultBrowser("http://www.erlang.org/doc/apps/wx/part_frame.html"),
      {noreply, State};
    ?wxID_ABOUT ->
      AboutString =
        "Demo of various widgets\n"
        "Authors: Olle & Dan",
      wxMessageDialog:showModal(wxMessageDialog:new(State#state.win, AboutString,
        [{style,
          ?wxOK bor
            ?wxICON_INFORMATION bor
            ?wxSTAY_ON_TOP},
          {caption, "About"}])),
      {noreply, State};
    ?wxID_EXIT ->
      wx_object:call(State#state.example, shutdown),
      {stop, normal, State};
    _ ->
      {noreply, State}
  end;
handle_event(#wx{event = #wxClose{}}, State = #state{win = Frame}) ->
  io:format("~p Closing window ~n", [self()]),
  ok = wxFrame:setStatusText(Frame, "Closing...", []),
  {stop, normal, State};
handle_event(Ev, State) ->
  io:format("~p Got event ~p ~n", [?MODULE, Ev]),
  {noreply, State}.

code_change(_, _, State) ->
  {stop, not_yet_implemented, State}.

terminate(_Reason, _State = #state{win = Frame}) ->
  %catch wx_object:call(State#state.example, shutdown),
  wxFrame:destroy(Frame),
  wx:destroy().


code_area(Parent) ->
  FixedFont = wxFont:new(10, ?wxFONTFAMILY_TELETYPE, ?wxNORMAL, ?wxNORMAL, []),
  Ed = wxStyledTextCtrl:new(Parent),

  ?stc:styleClearAll(Ed),
  ?stc:styleSetFont(Ed, ?wxSTC_STYLE_DEFAULT, FixedFont),
  ?stc:setLexer(Ed, ?wxSTC_LEX_ERLANG),
  ?stc:setMarginType(Ed, 0, ?wxSTC_MARGIN_NUMBER),
  LW = ?stc:textWidth(Ed, ?wxSTC_STYLE_LINENUMBER, "9"),
  ?stc:setMarginWidth(Ed, 0, LW),
  ?stc:setMarginWidth(Ed, 1, 0),

  ?stc:setSelectionMode(Ed, ?wxSTC_SEL_LINES),
  %%?stc:hideSelection(Ed, true),

  Styles = [{?wxSTC_ERLANG_DEFAULT, {0, 0, 0}},
    {?wxSTC_ERLANG_COMMENT, {160, 53, 35}},
    {?wxSTC_ERLANG_VARIABLE, {150, 100, 40}},
    {?wxSTC_ERLANG_NUMBER, {5, 5, 100}},
    {?wxSTC_ERLANG_KEYWORD, {130, 40, 172}},
    {?wxSTC_ERLANG_STRING, {170, 45, 132}},
    {?wxSTC_ERLANG_OPERATOR, {30, 0, 0}},
    {?wxSTC_ERLANG_ATOM, {0, 0, 0}},
    {?wxSTC_ERLANG_FUNCTION_NAME, {64, 102, 244}},
    {?wxSTC_ERLANG_CHARACTER, {236, 155, 172}},
    {?wxSTC_ERLANG_MACRO, {40, 144, 170}},
    {?wxSTC_ERLANG_RECORD, {40, 100, 20}},
    {?wxSTC_ERLANG_SEPARATOR, {0, 0, 0}},
    {?wxSTC_ERLANG_NODE_NAME, {0, 0, 0}},
    %% Optional 2.9 stuff
    {?wxSTC_ERLANG_COMMENT_FUNCTION, {160, 53, 35}},
    {?wxSTC_ERLANG_COMMENT_MODULE, {160, 53, 35}},
    {?wxSTC_ERLANG_COMMENT_DOC, {160, 53, 35}},
    {?wxSTC_ERLANG_COMMENT_DOC_MACRO, {160, 53, 35}},
    {?wxSTC_ERLANG_ATOM_QUOTED, {0, 0, 0}},
    {?wxSTC_ERLANG_MACRO_QUOTED, {40, 144, 170}},
    {?wxSTC_ERLANG_RECORD_QUOTED, {40, 100, 20}},
    {?wxSTC_ERLANG_NODE_NAME_QUOTED, {0, 0, 0}},
    {?wxSTC_ERLANG_BIFS, {130, 40, 172}},
    {?wxSTC_ERLANG_MODULES, {64, 102, 244}},
    {?wxSTC_ERLANG_MODULES_ATT, {64, 102, 244}}
  ],
  SetStyle = fun({Style, Color}) ->
    ?stc:styleSetFont(Ed, Style, FixedFont),
    ?stc:styleSetForeground(Ed, Style, Color)
  end,
  [SetStyle(Style) || Style <- Styles],
  ?stc:setKeyWords(Ed, 0, keyWords()),

  %% Scrolling
  Policy = ?wxSTC_CARET_SLOP bor ?wxSTC_CARET_JUMPS bor ?wxSTC_CARET_EVEN,
  ?stc:setYCaretPolicy(Ed, Policy, 3),
  ?stc:setVisiblePolicy(Ed, Policy, 3),

  ?stc:connect(Ed, stc_doubleclick),
  ?stc:setReadOnly(Ed, false),
  Ed.

load_code(Ed, {ok, Code}) ->
  ?stc:setReadOnly(Ed, false),
  ?stc:setTextRaw(Ed, <<Code/binary, 0:8>>),
  Lines = ?stc:getLineCount(Ed),
  Sz = trunc(math:log10(Lines)) + 1,
  LW = ?stc:textWidth(Ed, ?wxSTC_STYLE_LINENUMBER, lists:duplicate(Sz, $9)),
  %%io:format("~p ~p ~p~n", [Lines, Sz, LW]),
  ?stc:setMarginWidth(Ed, 0, LW + 5),
  ?stc:setReadOnly(Ed, false),
  Ed.

keyWords() ->
  L = ["after", "begin", "case", "try", "cond", "catch", "andalso", "orelse",
    "end", "fun", "if", "let", "of", "query", "receive", "when", "bnot", "not",
    "div", "rem", "band", "and", "bor", "bxor", "bsl", "bsr", "or", "xor"],
  lists:flatten([K ++ " " || K <- L] ++ [0]).

%% status(Frame, Format, Data) -> ok = wxFrame:setStatusText(Frame, Format, Data).

say(Frame, Message) ->
  Dialog = apply(wxMessageDialog, new, [Frame, force:to_list(Message)]),
  case wxMessageDialog:showModal(Dialog) of
    ?wxID_OK ->
      wxMessageDialog:destroy(Dialog),
      ok;
    _Any ->
      wxMessageDialog:destroy(Dialog),
      cancel
  end.