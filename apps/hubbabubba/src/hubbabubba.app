{application, hubbabubba,
 [{description, "PubSubHubbub Hub"},
  {vsn, "0.01"},
  {modules, [
    hub,
    hub_app,
    hub_sup,
    hub_storage
  ]},
  {registered, []},
  {mod, {hub_app, []}},
  {env, []},	  
  {applications, [kernel, stdlib, crypto]}]}.
