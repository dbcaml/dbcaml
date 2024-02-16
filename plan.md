## Connection 

on a success connection do the connection check in the state of the connection into a in memory table. When a connection is checkdout do it change it's connection to busy

```ocaml
let rec loop
  match recieve() with
  | Checkout -> 
      1. await connection which is free
      2. lock it.
      3. then return the connection to caller
  | Checkin ->
      1. Store connection/change state

  loop
```

Checkins and checkouts

### Connection
@callback connect(opts :: Keyword.t()) ::
              {:ok, state :: any} | {:error, Exception.t()}


@callback checkout(state :: any) ::
              {:ok, new_state :: any} | {:disconnect, Exception.t(), new_state :: any}

 @callback ping(state :: any) ::
              {:ok, new_state :: any} | {:disconnect, Exception.t(), new_state :: any}

@callback disconnect(err :: Exception.t(), state :: any) :: :ok

### Transactions

 @callback handle_begin(opts :: Keyword.t(), state :: any) ::
              {:ok, result, new_state :: any}
              | {:ok, query, result, new_state :: any}
              | {status, new_state :: any}
              | {:disconnect, Exception.t(), new_state :: any}
 
 @callback handle_commit(opts :: Keyword.t(), state :: any) ::
              {:ok, result, new_state :: any}
              | {status, new_state :: any}
              | {:disconnect, Exception.t(), new_state :: any}

 @callback handle_rollback(opts :: Keyword.t(), state :: any) ::
              {:ok, result, new_state :: any}
              | {status, new_state :: any}
              | {:disconnect, Exception.t(), new_state :: any}

  @callback handle_status(opts :: Keyword.t(), state :: any) ::
              {status, new_state :: any}
              | {:disconnect, Exception.t(), new_state :: any}

### Query

@callback handle_prepare(query, opts :: Keyword.t(), state :: any) ::
              {:ok, query, new_state :: any}
              | {:error | :disconnect, Exception.t(), new_state :: any}

 @callback handle_execute(query, params, opts :: Keyword.t(), state :: any) ::
              {:ok, query, result, new_state :: any}
              | {:error | :disconnect, Exception.t(), new_state :: any}

@callback handle_close(query, opts :: Keyword.t(), state :: any) ::
              {:ok, result, new_state :: any}
              | {:error | :disconnect, Exception.t(), new_state :: any}

// Cursors
@callback handle_declare(query, params, opts :: Keyword.t(), state :: any) ::
              {:ok, query, cursor, new_state :: any}
              | {:error | :disconnect, Exception.t(), new_state :: any}

@callback handle_fetch(query, cursor, opts :: Keyword.t(), state :: any) ::
              {:cont | :halt, result, new_state :: any}
              | {:error | :disconnect, Exception.t(), new_state :: any}

 @callback handle_deallocate(query, cursor, opts :: Keyword.t(), state :: any) ::
              {:ok, result, new_state :: any}
              | {:error | :disconnect, Exception.t(), new_state :: any}


#### Query execution
1. check out (acquire a connection) 
2. run query
3. Check in
