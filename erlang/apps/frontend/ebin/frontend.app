{application,frontend,
             [{description,"Front end application for querl"},
              {vsn,"1"},
              {modules,[frontend,frontend_app,frontend_sup,toppage_handler]},
              {registered,[]},
              {applications,[kernel,stdlib,cowboy,querl]},
              {mod,{frontend_app,[]}},
              {env,[]}]}.
