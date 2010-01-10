-record(hub_request, {callback,
		      mode,
		      topic,
		      verify,
		      lease_seconds=0,
		      secret,
		      verify_token,
		      url=[]}).

-record(subscription, {callback,
		       topic,
		       verified=false,
		       verifying=false,
		       lease_seconds=0,
		       secret,
		       verify_token}).
