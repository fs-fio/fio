namespace FSharp.FIO.Redis

open StackExchange.Redis

open System

/// <summary>
/// Redis operation errors.
/// </summary>
type RedisError =
    /// <summary>
    /// Failed to connect to Redis.
    /// </summary>
    | ConnectionFailed of endpoint: string * exn
    /// <summary>
    /// Connection was closed.
    /// </summary>
    | ConnectionClosed of string
    /// <summary>
    /// Command execution failed.
    /// </summary>
    | CommandFailed of command: string * exn
    /// <summary>
    /// Key was not found.
    /// </summary>
    | KeyNotFound of key: string
    /// <summary>
    /// Type mismatch for key.
    /// </summary>
    | TypeMismatch of key: string * expected: string
    /// <summary>
    /// Transaction was aborted.
    /// </summary>
    | TransactionAborted of exn
    /// <summary>
    /// Subscription failed.
    /// </summary>
    | SubscriptionFailed of channel: string * exn
    /// <summary>
    /// Stream operation failed.
    /// </summary>
    | StreamError of stream: string * exn
    /// <summary>
    /// Operation timed out.
    /// </summary>
    | Timeout of operation: string
    /// <summary>
    /// General Redis error.
    /// </summary>
    | GeneralError of exn

/// <summary>
/// Functions for working with RedisError.
/// </summary>
module RedisError =

    /// <summary>
    /// Converts an exception to a RedisError.
    /// </summary>
    /// <param name="exn">The exception to convert.</param>
    /// <returns>A GeneralError wrapping the exception.</returns>
    let fromException (exn: exn) : RedisError =
        GeneralError exn

    /// <summary>
    /// Converts a RedisError to an exception.
    /// </summary>
    /// <param name="err">The RedisError to convert.</param>
    /// <returns>An exception representing the error.</returns>
    let toException (err: RedisError) : exn =
        match err with
        | GeneralError exn -> exn
        | _ -> Exception(err.ToString())

/// <summary>
/// Configuration for Redis connection.
/// </summary>
type RedisConfig =
    {
        /// <summary>
        /// Redis endpoint(s) in host:port format.
        /// </summary>
        Endpoints: string list
        /// <summary>
        /// Password for authentication.
        /// </summary>
        Password: string option
        /// <summary>
        /// Database index (0-15).
        /// </summary>
        Database: int
        /// <summary>
        /// Connection timeout in milliseconds.
        /// </summary>
        ConnectTimeout: int
        /// <summary>
        /// Sync operation timeout in milliseconds.
        /// </summary>
        SyncTimeout: int
        /// <summary>
        /// Async operation timeout in milliseconds.
        /// </summary>
        AsyncTimeout: int
        /// <summary>
        /// Allow admin operations (FLUSHDB, etc).
        /// </summary>
        AllowAdmin: bool
        /// <summary>
        /// Use SSL/TLS.
        /// </summary>
        Ssl: bool
        /// <summary>
        /// Client name for identification.
        /// </summary>
        ClientName: string option
        /// <summary>
        /// Abort on connect fail.
        /// </summary>
        AbortOnConnectFail: bool
    }

/// <summary>
/// Functions for creating Redis configurations.
/// </summary>
module RedisConfig =

    /// <summary>
    /// Creates a default configuration with the given endpoint.
    /// </summary>
    /// <param name="endpoint">The Redis server endpoint.</param>
    /// <returns>A new RedisConfig with default settings.</returns>
    let create endpoint =
        {
            Endpoints = [ endpoint ]
            Password = None
            Database = 0
            ConnectTimeout = 5000
            SyncTimeout = 5000
            AsyncTimeout = 5000
            AllowAdmin = false
            Ssl = false
            ClientName = None
            AbortOnConnectFail = false
        }

    /// <summary>
    /// Creates a configuration from multiple endpoints.
    /// </summary>
    /// <param name="endpoints">The Redis cluster endpoints.</param>
    /// <returns>A new RedisConfig for cluster mode.</returns>
    let createCluster endpoints =
        { create (List.head endpoints) with Endpoints = endpoints }

    /// <summary>
    /// Sets the password.
    /// </summary>
    /// <param name="password">The authentication password.</param>
    /// <param name="config">The Redis configuration.</param>
    /// <returns>The updated configuration.</returns>
    let withPassword password config =
        { config with Password = Some password }

    /// <summary>
    /// Sets the database index.
    /// </summary>
    /// <param name="db">The database index.</param>
    /// <param name="config">The Redis configuration.</param>
    /// <returns>The updated configuration.</returns>
    let withDatabase db config =
        { config with Database = db }

    /// <summary>
    /// Sets the connect timeout in milliseconds.
    /// </summary>
    /// <param name="ms">The timeout in milliseconds.</param>
    /// <param name="config">The Redis configuration.</param>
    /// <returns>The updated configuration.</returns>
    let withConnectTimeout ms config =
        { config with ConnectTimeout = ms }

    /// <summary>
    /// Sets the sync timeout in milliseconds.
    /// </summary>
    /// <param name="ms">The timeout in milliseconds.</param>
    /// <param name="config">The Redis configuration.</param>
    /// <returns>The updated configuration.</returns>
    let withSyncTimeout ms config =
        { config with SyncTimeout = ms }

    /// <summary>
    /// Sets the async timeout in milliseconds.
    /// </summary>
    /// <param name="ms">The timeout in milliseconds.</param>
    /// <param name="config">The Redis configuration.</param>
    /// <returns>The updated configuration.</returns>
    let withAsyncTimeout ms config =
        { config with AsyncTimeout = ms }

    /// <summary>
    /// Enables admin operations.
    /// </summary>
    /// <param name="config">The Redis configuration.</param>
    /// <returns>The updated configuration.</returns>
    let withAllowAdmin config =
        { config with AllowAdmin = true }

    /// <summary>
    /// Enables SSL/TLS.
    /// </summary>
    /// <param name="config">The Redis configuration.</param>
    /// <returns>The updated configuration.</returns>
    let withSsl config =
        { config with Ssl = true }

    /// <summary>
    /// Sets the client name.
    /// </summary>
    /// <param name="name">The client name.</param>
    /// <param name="config">The Redis configuration.</param>
    /// <returns>The updated configuration.</returns>
    let withClientName name config =
        { config with ClientName = Some name }

    /// <summary>
    /// Sets abort on connect fail behavior.
    /// </summary>
    /// <param name="abort">Whether to abort on connect fail.</param>
    /// <param name="config">The Redis configuration.</param>
    /// <returns>The updated configuration.</returns>
    let withAbortOnConnectFail abort config =
        { config with AbortOnConnectFail = abort }

    /// <summary>
    /// Converts config to StackExchange.Redis ConfigurationOptions.
    /// </summary>
    /// <param name="config">The Redis configuration.</param>
    /// <returns>The StackExchange.Redis ConfigurationOptions.</returns>
    let toConfigurationOptions (config: RedisConfig) : ConfigurationOptions =
        let opts = ConfigurationOptions()
        for endpoint in config.Endpoints do
            opts.EndPoints.Add endpoint
        config.Password |> Option.iter (fun p -> opts.Password <- p)
        opts.DefaultDatabase <- Nullable config.Database
        opts.ConnectTimeout <- config.ConnectTimeout
        opts.SyncTimeout <- config.SyncTimeout
        opts.AsyncTimeout <- config.AsyncTimeout
        opts.AllowAdmin <- config.AllowAdmin
        opts.Ssl <- config.Ssl
        config.ClientName |> Option.iter (fun n -> opts.ClientName <- n)
        opts.AbortOnConnectFail <- config.AbortOnConnectFail
        opts

    /// <summary>
    /// Parses a connection string to ConfigurationOptions.
    /// </summary>
    /// <param name="connStr">The connection string.</param>
    /// <returns>The parsed ConfigurationOptions.</returns>
    let parseConnectionString (connStr: string) : ConfigurationOptions =
        ConfigurationOptions.Parse connStr

/// <summary>
/// Represents a Redis connection (multiplexer wrapper).
/// </summary>
type RedisConnection =
    internal {
        /// <summary>
        /// The underlying connection multiplexer.
        /// </summary>
        Multiplexer: IConnectionMultiplexer
        /// <summary>
        /// The Redis configuration.
        /// </summary>
        Config: RedisConfig
        /// <summary>
        /// The database index.
        /// </summary>
        Database: int
    }

/// <summary>
/// Functions for working with RedisValue conversions.
/// </summary>
module RedisValue =

    /// <summary>
    /// Converts RedisValue to string option.
    /// </summary>
    /// <param name="v">The RedisValue to convert.</param>
    /// <returns>Some string if not null, None otherwise.</returns>
    let toString (v: RedisValue) : string option =
        if v.IsNull then None
        else Some (v.ToString())

    /// <summary>
    /// Converts RedisValue to byte array option.
    /// </summary>
    /// <param name="v">The RedisValue to convert.</param>
    /// <returns>Some byte array if not null, None otherwise.</returns>
    let toBytes (v: RedisValue) : byte[] option =
        if v.IsNull then None
        else Some (v.Box() :?> byte[])

    /// <summary>
    /// Converts RedisValue to int64 option.
    /// </summary>
    /// <param name="v">The RedisValue to convert.</param>
    /// <returns>Some int64 if parseable, None otherwise.</returns>
    let toInt64 (v: RedisValue) : int64 option =
        if v.IsNull then None
        else
            let mutable result = 0L
            if v.TryParse(&result) then Some result
            else None

    /// <summary>
    /// Converts RedisValue to float option.
    /// </summary>
    /// <param name="v">The RedisValue to convert.</param>
    /// <returns>Some float if parseable, None otherwise.</returns>
    let toFloat (v: RedisValue) : float option =
        if v.IsNull then None
        else
            let mutable result = 0.0
            if v.TryParse(&result) then Some result
            else None

    /// <summary>
    /// Creates RedisValue from string.
    /// </summary>
    /// <param name="s">The string to convert.</param>
    /// <returns>A RedisValue containing the string.</returns>
    let ofString (s: string) : RedisValue =
        RedisValue.op_Implicit s

    /// <summary>
    /// Creates RedisValue from byte array.
    /// </summary>
    /// <param name="b">The byte array to convert.</param>
    /// <returns>A RedisValue containing the bytes.</returns>
    let ofBytes (b: byte[]) : RedisValue =
        RedisValue.op_Implicit b

    /// <summary>
    /// Creates RedisValue from int64.
    /// </summary>
    /// <param name="i">The int64 to convert.</param>
    /// <returns>A RedisValue containing the int64.</returns>
    let ofInt64 (i: int64) : RedisValue =
        RedisValue.op_Implicit i

    /// <summary>
    /// Creates RedisValue from float.
    /// </summary>
    /// <param name="f">The float to convert.</param>
    /// <returns>A RedisValue containing the float.</returns>
    let ofFloat (f: float) : RedisValue =
        RedisValue.op_Implicit f
