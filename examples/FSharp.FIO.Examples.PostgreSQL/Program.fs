/// <summary>
/// FSharp.FIO.PostgreSQL example demonstrating database operations with connection pooling.
/// </summary>
module private FSharp.FIO.Examples.PostgreSQL

open FSharp.FIO.DSL
open FSharp.FIO.App
open FSharp.FIO.PostgreSQL

/// <summary>
/// User domain model for database operations.
/// </summary>
type User = {
    Id: int
    Name: string
    Email: string
    Age: int option
}

/// <summary>
/// User parsing utilities for result set mapping.
/// </summary>
module User =

    /// <summary>
    /// Parses a User from a database result set row.
    /// </summary>
    let fromResultSet rs =
        { Id = Results.getValue<int> 0 rs
          Name = Results.getValue<string> 1 rs
          Email = Results.getValue<string> 2 rs
          Age = Results.getValueOption<int> 3 rs }

/// <summary>
/// PostgreSQL demo app showing CRUD operations and transactions.
/// </summary>
/// <param name="host">PostgreSQL server hostname.</param>
/// <param name="database">Database name to connect to.</param>
type private PostgreSQLApp(host, database) =
    inherit FIOApp<unit, PgError>()

    let config = {
        ConnectionString = sprintf "Host=%s;Database=%s;Username=postgres;Password=password" host database
        MinPoolSize = 5
        MaxPoolSize = 20
        ConnectionLifetime = 300
        CommandTimeout = 30
    }

    let initializeDatabase (pool: ConnectionPool) : FIO<unit, PgError> =
        fio {
            // Create users table
            let createTableSql = """
                CREATE TABLE IF NOT EXISTS users (
                    id SERIAL PRIMARY KEY,
                    name VARCHAR(100) NOT NULL,
                    email VARCHAR(100) UNIQUE NOT NULL,
                    age INTEGER
                )
            """

            let! _ = execute createTableSql pool
            let! _ = execute "DELETE FROM users" pool
            return ()
        }

    let getUserById (id: int) (pool: ConnectionPool) : FIO<User option, PgError> =
        fio {
            let sql = "SELECT id, name, email, age FROM users WHERE id = @id"
            let parameters = ["id" @= id]
            return! queryFirstWithParams sql parameters User.fromResultSet pool
        }

    let getAllUsers (pool: ConnectionPool) : FIO<User list, PgError> =
        fio {
            let sql = "SELECT id, name, email, age FROM users ORDER BY id"
            return! query sql User.fromResultSet pool
        }

    let getUsersByMinAge (minAge: int) (pool: ConnectionPool) : FIO<User list, PgError> =
        fio {
            let sql = """
                SELECT id, name, email, age
                FROM users
                WHERE age >= @minAge
                ORDER BY age DESC
            """
            let parameters = ["minAge" @= minAge]
            return! queryWithParams sql parameters User.fromResultSet pool
        }

    let createUser (name: string) (email: string) (age: int option) (pool: ConnectionPool) : FIO<int, PgError> =
        fio {
            let sql = """
                INSERT INTO users (name, email, age)
                VALUES (@name, @email, @age)
                RETURNING id
            """

            let parameters = [
                "name" @= name
                "email" @= email
                "age" @= (age |> Option.map box |> Option.defaultValue (box System.DBNull.Value))
            ]

            return! insertReturning<int> sql parameters pool
        }

    let updateUserAge (userId: int) (newAge: int) (pool: ConnectionPool) : FIO<int, PgError> =
        fio {
            let sql = "UPDATE users SET age = @age WHERE id = @id"
            let parameters = [
                "id" @= userId
                "age" @= newAge
            ]

            return! executeWithParams sql parameters pool
        }

    let deleteUser (userId: int) (pool: ConnectionPool) : FIO<int, PgError> =
        fio {
            let sql = "DELETE FROM users WHERE id = @id"
            let parameters = ["id" @= userId]

            return! executeWithParams sql parameters pool
        }

    let transferUserOwnership (fromEmail: string) (toEmail: string) (pool: ConnectionPool) : FIO<unit, PgError> =
        transaction (fun conn ->
            fio {
                let! sourceUser =
                    Query.queryFirstWithParams
                        "SELECT id, name, email, age FROM users WHERE email = @email"
                        ["email" @= fromEmail]
                        User.fromResultSet
                        conn

                match sourceUser with
                | Some user ->
                    let! _ =
                        Command.executeWithParams
                            "UPDATE users SET email = @newEmail WHERE id = @id"
                            ["id" @= user.Id; "newEmail" @= toEmail]
                            conn

                    return ()
                | None ->
                    return! FIO.fail (GeneralError (System.Exception($"User with email {fromEmail} not found")))
            }
        ) pool

    let postgresDemo =
        fio {
            printfn "=== FSharp.FIO.PostgreSQL Example ==="
            printfn ""

            printfn "Creating connection pool..."
            let! pool = Pool.create config

            printfn "Initializing database..."
            do! initializeDatabase pool

            printfn "\nCreating users..."
            let! userId1 = createUser "Alice" "alice@example.com" (Some 30) pool
            printfn $"Created user with ID: {userId1}"

            let! userId2 = createUser "Bob" "bob@example.com" (Some 25) pool
            printfn $"Created user with ID: {userId2}"

            let! userId3 = createUser "Charlie" "charlie@example.com" None pool
            printfn $"Created user with ID: {userId3}"

            printfn "\nAll users:"
            let! users = getAllUsers pool
            for user in users do
                let ageStr = user.Age |> Option.map string |> Option.defaultValue "N/A"
                printfn $"  [{user.Id}] {user.Name} ({user.Email}) - Age: {ageStr}"

            printfn "\nUsers aged 25 or older:"
            let! olderUsers = getUsersByMinAge 25 pool
            for user in olderUsers do
                let ageStr = user.Age |> Option.map string |> Option.defaultValue "N/A"
                printfn $"  [{user.Id}] {user.Name} - Age: {ageStr}"

            printfn "\nUpdating Charlie's age to 28..."
            let! _ = updateUserAge userId3 28 pool
            let! charlie = getUserById userId3 pool
            match charlie with
            | Some user ->
                let ageStr = user.Age |> Option.map string |> Option.defaultValue "N/A"
                printfn $"  Updated: {user.Name} - Age: {ageStr}"
            | None -> printfn "  User not found"

            printfn "\nTransferring Alice's account to new email..."
            do! transferUserOwnership "alice@example.com" "alice.new@example.com" pool
            let! alice = getUserById userId1 pool
            match alice with
            | Some user ->
                printfn $"  New email: {user.Email}"
            | None -> printfn "  User not found"

            printfn "\nDeleting Bob..."
            let! _ = deleteUser userId2 pool
            let! remainingUsers = getAllUsers pool
            printfn $"  Remaining users: {remainingUsers.Length}"

            printfn "\n=== Demo Complete ==="

            printfn "\nClosing connection pool..."
            do! Pool.close pool

            return ()
        }

    override _.effect =
        postgresDemo

[<EntryPoint>]
let main _ =
    printfn "Note: This example requires a running PostgreSQL instance."
    printfn "To start one with Docker:"
    printfn "  docker run --name postgres-demo -e POSTGRES_PASSWORD=password -p 5432:5432 -d postgres"
    printfn "  docker exec -it postgres-demo psql -U postgres -c \"CREATE DATABASE testdb;\""
    printfn ""
    let host = "localhost"
    let database = "testdb"
    PostgreSQLApp(host, database).Run()
