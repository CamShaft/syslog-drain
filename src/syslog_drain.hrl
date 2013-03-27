
-record(handlers, {
  parsers :: [module()],
  mappers :: [module()],
  emitters :: [module()]
}).
