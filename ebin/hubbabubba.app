%% -*- erlang -*-

{application, hubbabubba,
 [
  {description, "PubSubHubbub Hub"},
  {vsn, "0.1"},
  {modules, [
	     hubbabubba,
	     hubbabubba_app,
	     hubbabubba_sup,
	     hubbabubba_web,
	     hubbabubba_resource,
	     hubbabubba_utils,
	     hubbabubba_req_parser,
	     hubbabubba_server
	    ]},
  {registered, []},
  {applications, [
            	  kernel,
                  stdlib,
		  sasl,
		  crypto,
		  webmachine,
		  ibrowse
                 ]},
  {mod, { hubbabubba_app, []}},
  {env, []}
 ]}.
