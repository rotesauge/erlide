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
-include_lib("erlide.hrl").

-export([start/0, start/1, start_link/0, start_link/1, format/3,
  init/1, terminate/2, code_change/3,
  handle_info/2, handle_call/3, handle_cast/2, handle_event/2, say/2]).



%% For wx-2.9 usage

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


%% init(Options) ->
%%   App = application:get_application(),
%%   io:format("~p~n",[App]),
%%   init_ets(),
%%   wx:new(Options),
%%   process_flag(trap_exit, true),
%%   Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Erlang IDE", [{size, {1000, 500}}]),
%%   MB = wxMenuBar:new(),
%%   File = wxMenu:new([]),
%%   wxMenu:append(File, ?NEWFILE, "&New file"),
%%   wxMenu:appendSeparator(File),
%%   wxMenu:append(File, ?LOADFILE, "&Load file"),
%%   wxMenu:appendSeparator(File),
%%   wxMenu:append(File, ?SAVEFILE, "&Save file"),
%%   wxMenu:appendSeparator(File),
%%   wxMenu:append(File, ?CLOSEFILE, "&Close file"),
%%   wxMenu:appendSeparator(File),
%%   wxMenu:append(File, ?wxID_PRINT, "&Print code"),
%%   wxMenu:appendSeparator(File),
%%   wxMenu:append(File, ?wxID_EXIT, "&Quit"),
%%   Help = wxMenu:new([]),
%%   wxMenu:append(Help, ?wxID_HELP, "Help"),
%%   wxMenu:append(Help, ?wxID_ABOUT, "About"),
%%   wxMenuBar:append(MB, File, "&File"),
%%   wxMenuBar:append(MB, Help, "&Help"),
%%   wxFrame:setMenuBar(Frame, MB),
%%   wxFrame:connect(Frame, command_menu_selected),
%%   wxFrame:connect(Frame, close_window),
%%   _SB = wxFrame:createStatusBar(Frame, []),
%%   Files = wxNotebook:new(Frame, 1, [{style, ?wxBK_DEFAULT}]),
%%   ets:insert(ide_state, #state{id = 1, win= Frame, tabs=Files}),
%%   wxFrame:show(Frame),
%%   %**********
%%   State=first_load(),
%%   %**********
%% %%   load_code(Code, {ok, <<"-module(erlide_app).\n-author(\"redeye\").\n-behaviour(application).\n%% Application callbacks\n-export([start/2,stop/1]).">>}),
%%   {Frame, State}.


init(Options) ->
  App = application:get_application(),
  io:format("~p~n",[App]),
  wx:new(Options),
  process_flag(trap_exit, true),
  MainWindow = wxFrame:new(wx:null(), ?wxID_ANY, "Erlang IDE", [{size, {1000, 500}}]),
  MB = wxMenuBar:new(),
  File = wxMenu:new([]),
  wxMenu:append(File, ?NEWFILE, "&New file"),
  wxMenu:appendSeparator(File),
  wxMenu:append(File, ?LOADFILE, "&Load file"),
  wxMenu:appendSeparator(File),
  wxMenu:append(File, ?SAVEFILE, "&Save file"),
  wxMenu:appendSeparator(File),
  wxMenu:append(File, ?CLOSEFILE, "&Close file"),
  wxMenu:appendSeparator(File),
  wxMenu:append(File, ?wxID_PRINT, "&Print code"),
  wxMenu:appendSeparator(File),
  wxMenu:append(File, ?wxID_EXIT, "&Quit"),
  Help = wxMenu:new([]),
  wxMenu:append(Help, ?wxID_HELP, "Help"),
  wxMenu:append(Help, ?wxID_ABOUT, "About"),
  Debug    = wxMenu:new([]),
  wxMenu:appendRadioItem(Debug, ?DEBUG_NONE, "None"),
  wxMenu:appendRadioItem(Debug, ?DEBUG_VERBOSE, "Verbose"),
  wxMenu:appendRadioItem(Debug, ?DEBUG_TRACE, "Trace"),
  wxMenu:appendRadioItem(Debug, ?DEBUG_DRIVER, "Driver"),
  wxMenuBar:append(MB, File, "&File"),
  wxMenuBar:append(MB, Debug, "&Debug"),
  wxMenuBar:append(MB, Help, "&Help"),
  wxFrame:setMenuBar(MainWindow, MB),
  wxFrame:connect(MainWindow, command_menu_selected),
  wxFrame:connect(MainWindow, close_window),
  _SB = wxFrame:createStatusBar(MainWindow, []),
  Panel = wxPanel:new(MainWindow, []),
  MainSizer = wxBoxSizer:new(?wxVERTICAL),
  Manager = wxAuiManager:new([{managed_wnd, Panel}]),
  Pane = ?pi:new(),
  ?pi:closeButton(Pane),
  ?pi:right(Pane),
  ?pi:dockable(Pane, [{b, true}]),
  ?pi:floatingSize(Pane, 300,200),
  ?pi:minSize(Pane, {50,50}),
  ?pi:paneBorder(Pane),
  ?pi:floatable(Pane, [{b, true}]),
  Pane2 = wxAuiPaneInfo:new(Pane),
  ?pi:centrePane(Pane2),
  Notebook = create_notebook(Panel, Manager, ?pi:new(Pane2)),
  wxPanel:setSizer(Panel, MainSizer),
  wxAuiManager:connect(Manager, aui_pane_button, [{skip,true}]),
  wxAuiManager:connect(Manager, aui_pane_maximize, [{skip,true}]),
  wxAuiManager:update(Manager),
  process_flag(trap_exit, true),
  Files = first_load(Notebook),
  wxFrame:show(MainWindow),
  Cur = lists:last(Files),
  State = #state{id = 1, mainwindow  = MainWindow,mainpanel = Panel,aui = Manager,notebook = Notebook,curfile = Cur,files =Files },
 %%   load_code(Code, {ok, <<"-module(erlide_app).\n-author(\"redeye\").\n-behaviour(application).\n%% Application callbacks\n-export([start/2,stop/1]).">>}),
  {MainWindow, State}.


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

handle_call(shutdown, _From, State=#state{mainpanel=Panel, aui=Manager}) ->
  wxAuiManager:unInit(Manager),
  wxAuiManager:destroy(Manager),
  wxPanel:destroy(Panel),
  {stop, normal, ok, State};
handle_call(Msg, _From, State) ->
  io:format("Got Call ~p~n", [Msg]),
  {reply, ok, State}.

handle_cast(Msg, State) ->
  io:format("Got cast ~p~n", [Msg]),
  {noreply, State}.

%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{obj = Notebook,event = #wxCommand{type = command_button_clicked}}, State = #state{mainwindow =  _Mainwindow}) ->
  Tab = wxPanel:new(Notebook, []),
  wxButton:new(Tab, ?wxID_ANY, [{label,"New tab"}]),
  wxAuiNotebook:insertPage(Notebook, 1, Tab, "OMG TAB!! ", [{select, false}]),
  {noreply, State};
handle_event(#wx{obj = Notebook,event = #wxAuiNotebook{type = command_auinotebook_page_changed, selection = Sel}},  State = #state{mainwindow =  Mainwindow ,files = Files}) ->
  Title= wxAuiNotebook:getPageText(Notebook, Sel),
  wxFrame:setStatusText(Mainwindow,Title),
  Listo =  [F||F<-Files,F#code_tab.file == Title],
  case Listo of
    []     ->  {noreply,State#state{selection = Sel}};
    [H|_T] ->  {noreply,State#state{ curfile = H,selection = Sel}}
  end;
handle_event(#wx{event = #wxAuiNotebook{type = command_auinotebook_page_close}},  State = #state{files = Files,curfile = #code_tab{file = File}}) ->
  Listo =  [F||F<-Files,F#code_tab.file =/= File],
  case Listo of
    [] -> {noreply,State};
    List = [H|_T]-> Newstate = State#state{ curfile = H,files = List},
    {noreply,Newstate}
  end;
handle_event(#wx{event = #wxAuiManager{type = aui_pane_button,
  button = Button}}, State) ->
  case Button of
    ?wxAUI_BUTTON_CLOSE ->ok;
%%       demo:format(State#state.config, "You have closed a pane.\n",[]);
    ?wxAUI_BUTTON_MAXIMIZE_RESTORE ->
      ok;
    ?wxAUI_BUTTON_PIN ->ok
%%       demo:format(State#state.config, "You have pinned a pane.\n",[])
  end,
  {noreply, State};
handle_event(#wx{event = #wxAuiManager{type = aui_pane_maximize}}, State) ->
%%   demo:format(State#state.config, "You have maximized a pane.\n",[]),
  {noreply, State};
handle_event(#wx{event = #wxAuiManager{type = aui_pane_restore}}, State) ->
%%   demo:format(State#state.config, "You have restored a pane.\n",[]),
  {noreply, State};
handle_event(#wx{id = Id, event = #wxCommand{type = command_menu_selected}}, State = #state{mainpanel =  Panel, curfile = #code_tab{file = File,code = Code,tab = Tab},notebook = Notebook,files = Files,mainwindow = Frame}) ->
  case Id of
    ?NEWFILE ->
      NewCurrent =  add_new_file(Notebook,getnewname(Files)),
      {noreply, State#state{curfile = NewCurrent,files = lists:merge([Files,[NewCurrent]])}};
    ?CLOSEFILE ->
      wxNotebook:destroy(Code);
    ?SAVEFILE ->
      case File of
        undefined -> {noreply, State};
        NotEmpty ->
          case file:read_file(NotEmpty) of
            {ok, _Text} ->
              TEXT = wxStyledTextCtrl:getText(Code),
              file:write_file(NotEmpty, unicode:characters_to_binary(TEXT)),
              {noreply, State};
            _Other    ->
              Dialog = apply(wxFileDialog, new, [Panel, []]),
              case wxFileDialog:showModal(Dialog) of
                ?wxID_OK ->
                  FName = wxFileDialog:getPath(Dialog),
                  TEXT = wxStyledTextCtrl:getText(Code),
                  file:write_file(FName, unicode:characters_to_binary(TEXT)),
                  ListWithout =  [F||F<-Files,F#code_tab.file =/= File],
                  NewCurrent =  #code_tab{file = FName,code = Code,tab = Tab},
                  wxFileDialog:destroy(Dialog),
                  wxAuiNotebook:setPageText(Notebook,wxAuiNotebook:getSelection(Notebook),FName),
                  {noreply, State#state{curfile = NewCurrent,files = lists:merge([ListWithout,[NewCurrent]])}};
                _Any ->
                  wxFileDialog:destroy(Dialog),
                  {noreply, State}
              end
          end
      end;
    ?LOADFILE ->
      Dialog = apply(wxFileDialog, new, [Panel, []]),
      case wxFileDialog:showModal(Dialog) of
        ?wxID_OK ->
          FName = wxFileDialog:getPath(Dialog),
          NewsF=add_new_file(Notebook,FName),
          Newstate=State#state{curfile = NewsF,files = lists:merge([Files,[NewsF]])},
          wxFileDialog:destroy(Dialog),
          {noreply, Newstate};
        _Any ->
          wxFileDialog:destroy(Dialog),
          {noreply, State}
      end;
    ?wxID_PRINT ->
      %% If you are going to printout mainly text it is easier if
      %% you generate HTML code and use a wxHtmlEasyPrint
      %% instead of using DCs
      Module = File,
      HEP = wxHtmlEasyPrinting:new([{name, "Print"},
        {parentWindow, State#state.win}]),
      Html = demo_html_tagger:erl2htmltext(Module),
      wxHtmlEasyPrinting:previewText(HEP, Html),
      {noreply, State};
    ?DEBUG_TRACE ->
      wx:debug(trace),
      {noreply, State};
    ?DEBUG_DRIVER ->
      wx:debug(driver),
      {noreply, State};
    ?DEBUG_VERBOSE ->
      wx:debug(verbose),
      {noreply, State};
    ?DEBUG_NONE ->
      wx:debug(none),
      {noreply, State};
    ?wxID_HELP ->
      HelpString =
        "ERLIDE`s help\n"
        "Sorry, help is under construction",
      wxMessageDialog:showModal(wxMessageDialog:new(Frame, HelpString,[{style,?wxOK bor ?wxICON_INFORMATION bor ?wxSTAY_ON_TOP}, {caption, "Help"}])),
      {noreply, State};
    ?wxID_ABOUT ->
      AboutString = "ERLIDE ~n Author: rotesauge aka RedEye",
      wxMessageDialog:showModal(wxMessageDialog:new(Frame, AboutString,
        [{style,
          ?wxOK bor
            ?wxICON_INFORMATION bor
            ?wxSTAY_ON_TOP},
          {caption, "About"}])),
      {noreply, State};
    ?wxID_EXIT ->
      {stop, normal, State};
    _ ->
      {noreply, State}
  end;
handle_event(#wx{event = #wxClose{}}, State = #state{mainwindow = Frame}) ->
  ok = wxFrame:setStatusText(Frame, "Closing...", []),
  {stop, normal, State};
handle_event(_Ev = #wx{}, State) ->
%%   io:format("~p Got event ~p ~n", [?MODULE, Ev]),
%%   io:format("Got event  ~n"),
  {noreply, State}.



code_change(_, _, State) ->
  {stop, not_yet_implemented, State}.

terminate(_Reason, _State = #state{win = Frame,files = Files}) ->
  application:set_env(group_leader(),openfiles,[F||{{_,_},F}<-Files]),
  wxFrame:destroy(Frame),
  wx:destroy().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getnewname([])->"NewFile.erl";
getnewname(Files)->getnewname(Files,"NewFile",".erl",0).
getnewname(Files,Name,Ext,Num) when Num == 0 ->
  NewFName =  lists:flatten([Name,Ext]),
  case [F||F<-Files,F#code_tab.file == NewFName] of
    [] -> NewFName;
    _A -> getnewname(Files,Name,Ext,Num+1)
  end;
getnewname(Files,Name,Ext,Num) ->
  NewFName = lists:flatten([Name,integer_to_list(Num),Ext]),
    case [F||F<-Files,F#code_tab.file == NewFName] of
    [] -> NewFName;
    _A -> getnewname(Files,Name,Ext,Num+1)
  end.

create_notebook(Parent, Manager, Pane) ->
  Style = (0
    bor ?wxAUI_NB_DEFAULT_STYLE
    bor ?wxAUI_NB_TOP
    bor ?wxAUI_NB_WINDOWLIST_BUTTON
    bor ?wxAUI_NB_CLOSE_ON_ACTIVE_TAB
    bor ?wxAUI_NB_TAB_MOVE
    bor ?wxAUI_NB_SCROLL_BUTTONS
  ),

  Notebook = wxAuiNotebook:new(Parent, [{style, Style}]),

%%   Tab1 = wxPanel:new(Notebook, []),
%%   wxPanel:setBackgroundColour(Tab1, ?wxBLACK),
%%   wxButton:new(Tab1, ?wxID_ANY, [{label,"New tab"}]),
%%   wxAuiNotebook:addPage(Notebook, Tab1, "You can", []),
%%
%%   Tab2 = wxPanel:new(Notebook, []),
%%   wxPanel:setBackgroundColour(Tab2, ?wxRED),
%%   wxButton:new(Tab2, ?wxID_ANY, [{label,"New tab"}]),
%%   wxAuiNotebook:addPage(Notebook, Tab2, "rearrange", []),
%%
%%   Tab3 = wxPanel:new(Notebook, []),
%%   wxPanel:setBackgroundColour(Tab3, ?wxGREEN),
%%   wxButton:new(Tab3, ?wxID_ANY, [{label,"New tab"}]),
%%   wxAuiNotebook:addPage(Notebook, Tab3, "these tabs", []),

  wxAuiManager:addPane(Manager, Notebook, Pane),

  wxAuiNotebook:connect(Notebook, command_button_clicked),
  wxAuiNotebook:connect(Notebook, command_auinotebook_page_close, [{skip, false}]),
  wxAuiNotebook:connect(Notebook, command_auinotebook_page_changed),
  Notebook.



first_load(Notebook)->
  {ok,OpenFiles}   = application:get_env(openfiles),
  case OpenFiles of
    []  ->
      [add_new_file(Notebook,"NewFile.erl")];
    List->
      [add_new_file(Notebook,F)||F<-List]
  end.

add_new_file(Notebook,Fname)->
  ForcedFname = force:to_list(Fname),
  Tab = wxPanel:new(Notebook, []),
  Code = code_area(Tab),
  MainSizer = wxBoxSizer:new(?wxVERTICAL),
  wxPanel:setSizer(Tab, MainSizer),
  wxAuiNotebook:addPage(Notebook, Tab,ForcedFname, [{select, true}]),
  case file:read_file(Fname) of
    {ok, Text} ->
      load_code(Code, {ok, Text}),
     #code_tab {tab = Tab,code = Code,file = ForcedFname};
     _Other    -> #code_tab {tab = Tab,code = Code,file = ForcedFname}%{{error,cantreadfile},ForcedFname}
  end.


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

unload_code(Ed) ->
  ?stc:setReadOnly(Ed, false),
  ?stc:setTextRaw(Ed, <<0:8>>),
  ?stc:setReadOnly(Ed, true),
  Ed.

find(Ed) ->
  ?stc:searchAnchor(Ed),
  Res = ?stc:searchNext(Ed, ?wxSTC_FIND_REGEXP, "^init"),
  case Res >= 0 of
    true ->
      %% io:format("Found ~p ~n",[Res]),
      ?stc:scrollToLine(Ed,?stc:lineFromPosition(Ed,Res) - 1),
      true;
    false ->
      io:format("Not Found ~s ~n",["^init"]),
      false
  end.
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