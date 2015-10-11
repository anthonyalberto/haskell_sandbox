-- Set prompt :
:set prompt "ghci> "

-- Load a module in ghci :
:module + Data.Ratio
-- or
:m +Data.Ratio

-- examine method definition with more info than :t
:info <method>

-- print more type info
:set +t

-- turn off automatic :t
:unset +t

-- result of last command
it

-- print help
:?

-- show all vars declared in the console session
:show bindings
