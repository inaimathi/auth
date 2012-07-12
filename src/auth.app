{application, auth,
 [{description, "Production-worthy, minimal, generic authentication system"},
  {vsn, "1.0"},
  {modules, [auth_app, auth_sup, m2crypto, users, rsa_auth]},
  {registered, [users, m2crypto]},
  {applications, [kernel, stdlib, sasl, mnesia]},
  {mod, {auth_app, []}},
  {start_phases, []}]}.