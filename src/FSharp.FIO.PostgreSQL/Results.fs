/// <summary>
/// Result set handling and mapping for PostgreSQL queries.
/// </summary>
namespace FSharp.FIO.PostgreSQL

open FSharp.FIO.DSL

open Npgsql
open System
open System.Data

/// <summary>
/// Functions for working with query result sets.
/// </summary>
[<RequireQualifiedAccess>]
module Results =

    /// <summary>
    /// Creates a result set from an NpgsqlDataReader.
    /// </summary>
    /// <param name="reader">The NpgsqlDataReader to wrap.</param>
    let internal create (reader: NpgsqlDataReader) : ResultSet =
        { Reader = reader }

    /// <summary>
    /// Gets the value of a column by index.
    /// </summary>
    /// <param name="index">The zero-based column index.</param>
    /// <param name="resultSet">The result set to read from.</param>
    let getValue<'T> (index: int) (resultSet: ResultSet) : 'T =
        if resultSet.Reader.IsDBNull(index) then
            Unchecked.defaultof<'T>
        else
            resultSet.Reader.GetFieldValue<'T>(index)

    /// <summary>
    /// Gets the value of a column by name.
    /// </summary>
    /// <param name="name">The column name.</param>
    /// <param name="resultSet">The result set to read from.</param>
    let getValueByName<'T> (name: string) (resultSet: ResultSet) : 'T =
        let index = resultSet.Reader.GetOrdinal(name)
        getValue<'T> index resultSet

    /// <summary>
    /// Gets an optional value of a column by index.
    /// </summary>
    /// <param name="index">The zero-based column index.</param>
    /// <param name="resultSet">The result set to read from.</param>
    let getValueOption<'T> (index: int) (resultSet: ResultSet) : 'T option =
        if resultSet.Reader.IsDBNull(index) then
            None
        else
            Some (resultSet.Reader.GetFieldValue<'T>(index))

    /// <summary>
    /// Gets an optional value of a column by name.
    /// </summary>
    /// <param name="name">The column name.</param>
    /// <param name="resultSet">The result set to read from.</param>
    let getValueOptionByName<'T> (name: string) (resultSet: ResultSet) : 'T option =
        let index = resultSet.Reader.GetOrdinal(name)
        getValueOption<'T> index resultSet

    /// <summary>
    /// Reads all rows from the result set using a mapping function.
    /// </summary>
    /// <param name="mapper">Function to map each row to a value.</param>
    /// <param name="resultSet">The result set to read from.</param>
    let readAll<'T> (mapper: ResultSet -> 'T) (resultSet: ResultSet) : FIO<'T list, exn> =
        FIO.Attempt(
            (fun () ->
                let results = ResizeArray<'T>()
                while resultSet.Reader.Read() do
                    results.Add(mapper resultSet)
                List.ofSeq results),
            id)

    /// <summary>
    /// Reads the first row from the result set, if any.
    /// </summary>
    /// <param name="mapper">Function to map the row to a value.</param>
    /// <param name="resultSet">The result set to read from.</param>
    let readFirst<'T> (mapper: ResultSet -> 'T) (resultSet: ResultSet) : FIO<'T option, exn> =
        FIO.Attempt(
            (fun () ->
                if resultSet.Reader.Read() then
                    Some (mapper resultSet)
                else
                    None),
            id)

    /// <summary>
    /// Reads a single row from the result set.
    /// Fails if there are zero or more than one row.
    /// </summary>
    /// <param name="mapper">Function to map the row to a value.</param>
    /// <param name="resultSet">The result set to read from.</param>
    let readSingle<'T> (mapper: ResultSet -> 'T) (resultSet: ResultSet) : FIO<'T, exn> =
        FIO.Attempt(
            (fun () ->
                if resultSet.Reader.Read() then
                    let result = mapper resultSet
                    if resultSet.Reader.Read() then
                        failwith "Expected single row, but found multiple rows"
                    else
                        result
                else
                    failwith "Expected single row, but found no rows"),
            id)

    /// <summary>
    /// Closes the result set reader.
    /// </summary>
    /// <param name="resultSet">The result set to close.</param>
    let close (resultSet: ResultSet) : FIO<unit, exn> =
        FIO.Attempt(
            (fun () ->
                if not resultSet.Reader.IsClosed then
                    resultSet.Reader.Close()),
            id)

    /// <summary>
    /// Gets an int value by column index.
    /// </summary>
    let getInt = getValue<int>

    /// <summary>
    /// Gets a long value by column index.
    /// </summary>
    let getLong = getValue<int64>

    /// <summary>
    /// Gets a string value by column index.
    /// </summary>
    let getString = getValue<string>

    /// <summary>
    /// Gets a bool value by column index.
    /// </summary>
    let getBool = getValue<bool>

    /// <summary>
    /// Gets a decimal value by column index.
    /// </summary>
    let getDecimal = getValue<decimal>

    /// <summary>
    /// Gets a DateTime value by column index.
    /// </summary>
    let getDateTime = getValue<DateTime>

    /// <summary>
    /// Gets a Guid value by column index.
    /// </summary>
    let getGuid = getValue<Guid>

    /// <summary>
    /// Gets an optional int value by column index.
    /// </summary>
    let getIntOption = getValueOption<int>

    /// <summary>
    /// Gets an optional long value by column index.
    /// </summary>
    let getLongOption = getValueOption<int64>

    /// <summary>
    /// Gets an optional string value by column index.
    /// </summary>
    let getStringOption = getValueOption<string>

    /// <summary>
    /// Gets an optional bool value by column index.
    /// </summary>
    let getBoolOption = getValueOption<bool>

    /// <summary>
    /// Gets an optional decimal value by column index.
    /// </summary>
    let getDecimalOption = getValueOption<decimal>

    /// <summary>
    /// Gets an optional DateTime value by column index.
    /// </summary>
    let getDateTimeOption = getValueOption<DateTime>

    /// <summary>
    /// Gets an optional Guid value by column index.
    /// </summary>
    let getGuidOption = getValueOption<Guid>
