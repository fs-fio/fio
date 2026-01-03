/// <summary>
/// Example usage of FSharp.FIO.PostgreSQL library.
///
/// This example demonstrates:
/// - Connection pool creation and management
/// - Query execution with result mapping
/// - Parameterized queries for SQL injection prevention
/// - Transaction management with automatic commit/rollback
/// - INSERT operations with RETURNING clause
/// - Error handling with FIO effects
///
/// Note: This example requires a running PostgreSQL instance.
/// To run with a local PostgreSQL:
///   docker run --name postgres-demo -e POSTGRES_PASSWORD=password -p 5432:5432 -d postgres
/// </summary>
module FSharp.FIO.Examples.PostgreSQL.Program

open FSharp.FIO.DSL
open FSharp.FIO.App
open FSharp.FIO.Experimental.PostgreSQL

// Domain model
type User = {
    Id: int
    Name: string
    Email: string
    Age: int option
}

// Configuration
let config = {
    ConnectionString = "Host=localhost;Database=testdb;Username=postgres;Password=password"
    MinPoolSize = 5
    MaxPoolSize = 20
    ConnectionLifetime = 300
    CommandTimeout = 30
}

// Database initialization
let initializeDatabase (pool: ConnectionPool) : FIO<unit, exn> =
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

        let! _ = Dsl.execute createTableSql pool

        // Clear existing data for demo
        let! _ = Dsl.execute "DELETE FROM users" pool

        return ()
    }

// Query examples
let getUserById (id: int) (pool: ConnectionPool) : FIO<User option, exn> =
    fio {
        let sql = "SELECT id, name, email, age FROM users WHERE id = @id"
        let parameters = ["id" @= id]

        let mapper (rs: ResultSet) = {
            Id = Results.getValue<int> 0 rs
            Name = Results.getValue<string> 1 rs
            Email = Results.getValue<string> 2 rs
            Age = Results.getValueOption<int> 3 rs
        }

        return! Dsl.queryFirstWithParams sql parameters mapper pool
    }

let getAllUsers (pool: ConnectionPool) : FIO<User list, exn> =
    fio {
        let sql = "SELECT id, name, email, age FROM users ORDER BY id"

        let mapper (rs: ResultSet) = {
            Id = Results.getValue<int> 0 rs
            Name = Results.getValue<string> 1 rs
            Email = Results.getValue<string> 2 rs
            Age = Results.getValueOption<int> 3 rs
        }

        return! Dsl.query sql mapper pool
    }

let getUsersByMinAge (minAge: int) (pool: ConnectionPool) : FIO<User list, exn> =
    fio {
        let sql = """
            SELECT id, name, email, age
            FROM users
            WHERE age >= @minAge
            ORDER BY age DESC
        """
        let parameters = ["minAge" @= minAge]

        let mapper (rs: ResultSet) = {
            Id = Results.getValue<int> 0 rs
            Name = Results.getValue<string> 1 rs
            Email = Results.getValue<string> 2 rs
            Age = Results.getValueOption<int> 3 rs
        }

        return! Dsl.queryWithParams sql parameters mapper pool
    }

// Command examples
let createUser (name: string) (email: string) (age: int option) (pool: ConnectionPool) : FIO<int, exn> =
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

        return! Dsl.insertReturning<int> sql parameters pool
    }

let updateUserAge (userId: int) (newAge: int) (pool: ConnectionPool) : FIO<int, exn> =
    fio {
        let sql = "UPDATE users SET age = @age WHERE id = @id"
        let parameters = [
            "id" @= userId
            "age" @= newAge
        ]

        return! Dsl.executeWithParams sql parameters pool
    }

let deleteUser (userId: int) (pool: ConnectionPool) : FIO<int, exn> =
    fio {
        let sql = "DELETE FROM users WHERE id = @id"
        let parameters = ["id" @= userId]

        return! Dsl.executeWithParams sql parameters pool
    }

// Transaction example
let transferUserOwnership (fromEmail: string) (toEmail: string) (pool: ConnectionPool) : FIO<unit, exn> =
    Dsl.transaction (fun conn ->
        fio {
            // Get source user
            let! sourceUser =
                Query.queryFirstWithParams
                    "SELECT id, name, email, age FROM users WHERE email = @email"
                    ["email" @= fromEmail]
                    (fun rs -> {
                        Id = Results.getValue<int> 0 rs
                        Name = Results.getValue<string> 1 rs
                        Email = Results.getValue<string> 2 rs
                        Age = Results.getValueOption<int> 3 rs
                    })
                    conn

            match sourceUser with
            | Some user ->
                // Update email
                let! _ =
                    Command.executeWithParams
                        "UPDATE users SET email = @newEmail WHERE id = @id"
                        ["id" @= user.Id; "newEmail" @= toEmail]
                        conn

                return ()
            | None ->
                return! FIO.Fail (System.Exception($"User with email {fromEmail} not found"))
        }
    ) pool

// Demo application
let demoEffect : FIO<unit, exn> =
    fio {
        printfn "=== FSharp.FIO.PostgreSQL Example ==="
        printfn ""

        // Create connection pool
        printfn "Creating connection pool..."
        let! pool = Pool.create config

        // Initialize database
        printfn "Initializing database..."
        do! initializeDatabase pool

        // Create users
        printfn "\nCreating users..."
        let! userId1 = createUser "Alice" "alice@example.com" (Some 30) pool
        printfn $"Created user with ID: {userId1}"

        let! userId2 = createUser "Bob" "bob@example.com" (Some 25) pool
        printfn $"Created user with ID: {userId2}"

        let! userId3 = createUser "Charlie" "charlie@example.com" None pool
        printfn $"Created user with ID: {userId3}"

        // Query all users
        printfn "\nAll users:"
        let! users = getAllUsers pool
        for user in users do
            let ageStr = user.Age |> Option.map string |> Option.defaultValue "N/A"
            printfn $"  [{user.Id}] {user.Name} ({user.Email}) - Age: {ageStr}"

        // Query users by age
        printfn "\nUsers aged 25 or older:"
        let! olderUsers = getUsersByMinAge 25 pool
        for user in olderUsers do
            let ageStr = user.Age |> Option.map string |> Option.defaultValue "N/A"
            printfn $"  [{user.Id}] {user.Name} - Age: {ageStr}"

        // Update user
        printfn "\nUpdating Charlie's age to 28..."
        let! _ = updateUserAge userId3 28 pool
        let! charlie = getUserById userId3 pool
        match charlie with
        | Some user ->
            let ageStr = user.Age |> Option.map string |> Option.defaultValue "N/A"
            printfn $"  Updated: {user.Name} - Age: {ageStr}"
        | None -> printfn "  User not found"

        // Transaction example
        printfn "\nTransferring Alice's account to new email..."
        do! transferUserOwnership "alice@example.com" "alice.new@example.com" pool
        let! alice = getUserById userId1 pool
        match alice with
        | Some user ->
            printfn $"  New email: {user.Email}"
        | None -> printfn "  User not found"

        // Delete user
        printfn "\nDeleting Bob..."
        let! _ = deleteUser userId2 pool
        let! remainingUsers = getAllUsers pool
        printfn $"  Remaining users: {remainingUsers.Length}"

        printfn "\n=== Demo Complete ==="

        // Clean up
        printfn "\nClosing connection pool..."
        do! Pool.close pool

        return ()
    }

// Application entry point
type PostgreSQLExampleApp() =
    inherit FIOApp<unit, exn>()
    override _.effect = demoEffect

[<EntryPoint>]
let main args =
    printfn "Note: This example requires a running PostgreSQL instance."
    printfn "To start one with Docker:"
    printfn "  docker run --name postgres-demo -e POSTGRES_PASSWORD=password -p 5432:5432 -d postgres"
    printfn "  docker exec -it postgres-demo psql -U postgres -c \"CREATE DATABASE testdb;\""
    printfn ""

    PostgreSQLExampleApp().Run()
