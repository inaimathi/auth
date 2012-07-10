{application, auth,
 [{description, "Production-worthy, minimal, generic authentication system"},
  {vsn, "1.0"},
  {modules, [auth_app, auth_sup, users]},
  {registered, [users]},
  {applications, [kernel, stdlib, sasl, mnesia]},
  {mod, {auth_app, []}},
  {start_phases, []}]}.