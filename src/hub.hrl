-record(hub_request, {callback,
		      mode,
		      topic,
		      verify,
		      lease_seconds=0,
		      secret,
		      verify_token}).

-record(subscription, {callback,
		       topic,
		       verified=false,
		       lease_seconds=0,
		       secret,
		       verify_token}).
