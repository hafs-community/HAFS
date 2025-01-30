import sqlite3
from typing import Any, List, Optional, Tuple

__all__ = ["SQLiteDB"]


class SQLiteDBError(Exception):
    """
    Base class for SQLiteDB exceptions.
    """
    OperationalError = sqlite3.OperationalError
    IntegrityError = sqlite3.IntegrityError


class SQLiteDB:
    """
    A class for interacting with an SQLite3 database.

    Parameters:
        db_name (str): The name of the SQLite database file.

    Attributes:
        db_name (str): The name of the SQLite database file.
        connection (sqlite3.Connection): The connection object for the database.

    """

    def __init__(self, db_name: str) -> None:
        self.db_name = db_name
        self.connection: Optional[sqlite3.Connection] = None

    def connect(self) -> None:
        """
        Connects to the SQLite database.

        """

        try:
            self.connection = sqlite3.connect(self.db_name)
        except sqlite3.OperationalError as exc:
            raise sqlite3.OperationalError(exc)

    def disconnect(self) -> None:
        """
        Disconnects from the SQLite database.

        """

        if self.connection:
            self.connection.close()

    def execute_query(self, query: str, params: Optional[Tuple[Any, ...]] = None) -> sqlite3.Cursor:
        """
        Executes an SQL query.

        Parameters:
            query (str): The SQL query to execute.
            params (tuple, optional): The parameters to be passed to the query.

        Returns:
            cursor (sqlite3.Cursor): The cursor object.

        """

        cursor = self.connection.cursor()
        if params:
            cursor.execute(query, params)
        else:
            cursor.execute(query)
        self.connection.commit()
        return cursor

    def create_table(self, table_name: str, columns: List[str]) -> None:
        """
        Creates a table in the database.

        Parameters:
            table_name (str): The name of the table to create.
            columns (list): The list of column definitions.

        """

        query = f"CREATE TABLE IF NOT EXISTS {table_name} ({', '.join(columns)})"
        self.execute_query(query)

    def add_column(self, table_name: str, column_name: str, column_type: str) -> None:
        """
        Adds a column to an existing table.

        Parameters:
            table_name (str): The name of the table.
            column_name (str): The name of the column to add.
            column_type (str): The data type of the column.

        """

        query = f"ALTER TABLE {table_name} ADD COLUMN {column_name} {column_type}"
        self.execute_query(query)

    def remove_column(self, table_name: str, column_name: str) -> None:
        """
        Removes a column from an existing table.

        Parameters:
            table_name (str): The name of the table.
            column_name (str): The name of the column to remove.

        """

        try:
            query = f"ALTER TABLE {table_name} DROP COLUMN {column_name}"
            self.execute_query(query)
        except sqlite3.OperationalError as exc:
            query = f"PRAGMA table_info({table_name})"
            cursor = self.execute_query(query)
            columns = [column[1] for column in cursor.fetchall()]
            if column_name not in columns:
                raise ValueError(f"Column '{column_name}' does not exist in table '{table_name}'")
            raise sqlite3.OperationalError(exc)

    def update_data(
        self,
        table_name: str,
        column_name: str,
        new_value: Any,
        condition_column: str,
        condition_value: Any
    ) -> None:
        """
        Updates data in a table.

        Parameters:
            table_name (str): The name of the table.
            column_name (str): The name of the column to update.
            new_value (any): The new value for the column.
            condition_column (str): The column to use for the condition.
            condition_value (any): The value to use in the condition.

        """

        query = f"UPDATE {table_name} SET {column_name} = ? WHERE {condition_column} = ?"
        self.execute_query(query, (new_value, condition_value))

    def insert_data(self, table_name: str, values: List[Any]) -> None:
        """
        Inserts data into a table.

        Parameters:
            table_name (str): The name of the table.
            values (list): The values to insert.

        """

        placeholders = ", ".join(["?"] * len(values))
        query = f"INSERT INTO {table_name} VALUES ({placeholders})"
        self.execute_query(query, values)

    def fetch_data(
        self,
        table_name: str,
        columns: Optional[List[str]] = None,
        condition: Optional[str] = None
    ) -> List[Tuple]:
        """
        Fetches data from a table.

        Parameters:
            table_name (str): The name of the table.
            columns (list, optional): The list of columns to fetch.
            condition (str, optional): The condition to use in the query.

        Returns:
            result (list): The fetched data.

        """

        column_names = "*" if not columns else ", ".join(columns)
        query = f"SELECT {column_names} FROM {table_name}"
        if condition:
            query += f" WHERE {condition}"
        cursor = self.execute_query(query)
        return cursor.fetchall()

    def remove_data(self, table_name: str, condition_column: str, condition_value: Any) -> None:
        """
        Removes data from a table.

        Parameters:
            table_name (str): The name of the table.
            condition_column (str): The column to use for the condition.
            condition_value (any): The value to use in the condition.

        """

        query = f"DELETE FROM {table_name} WHERE {condition_column} = ?"
        self.execute_query(query, (condition_value,))
