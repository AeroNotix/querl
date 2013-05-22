{application,frontend,
             [{description,"Front end application for querl"},
              {vsn,"1"},
              {modules,[frontend,frontend_app,frontend_sup,genreq,pop_handler,
                        push_handler]},
              {registered,[]},
              {applications,[kernel,stdlib,cowboy,querl]},
              {mod,{frontend_app,[]}},
              {env,[]}]}.
