%%%-------------------------------------------------------------------
%%% @author redreye
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. июн 2014 21:25
%%%-------------------------------------------------------------------
-author("redreye").


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

-define(DEBUG_NONE, 101).
-define(DEBUG_VERBOSE, 102).
-define(DEBUG_TRACE, 103).
-define(DEBUG_DRIVER, 104).
-define(CLOSEFILE, 105).
-define(SAVEFILE, 106).
-define(LOADFILE, 107).
-define(NEWFILE, 108).
-define(stc, wxStyledTextCtrl).
-define(pi, wxAuiPaneInfo).

-record(state, {id, win, tabs, curfile, curcode,mainwindow,mainpanel,aui,files,curent_tab,notebook,selection}).

-record(code_tab, {tab, code, file}).

