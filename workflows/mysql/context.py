from django import forms
import mysql.connector as sql

class DBConnection:
    def __init__(self, user, password, host, database):
        self.user = user
        self.password = password
        self.host = host
        self.database = database

    def cursor(self):
        self.con = sql.connect(user=self.user, password=self.password, host=self.host, database=self.database)
        return self.con.cursor()

    def close(self):
        self.con.close()

class DBContext:
    def __init__(self, connection):
        self.connection = connection
        cursor = connection.cursor()
        cursor.execute('SHOW tables')
        self.tables = [table for (table,) in cursor]
        self.cols = {}
        for table in self.tables:
            cursor.execute("SELECT column_name FROM information_schema.columns WHERE table_name = '%s'" % table)
            self.cols[table] = [col for (col,) in cursor]
        self.connected = {}
        cursor.execute(
           "SELECT table_name, column_name, referenced_table_name, referenced_column_name \
            FROM information_schema.KEY_COLUMN_USAGE \
            WHERE referenced_table_name IS NOT NULL")
        for (table, col, ref_table, ref_col) in cursor:
            self.connected[(table, ref_table)] = (col, ref_col)
        self.target_table = self.tables[0]
        self.connection.close()

    def update(self, postdata):
        widget_id = postdata.get('widget_id')[0]
        self.target_table = postdata.get('target_table%s' % widget_id)[0]
        self.tables = postdata.get('tables%s' % widget_id)
        # Propagate the selected tables
        for table in self.cols.keys():
            if table not in self.tables:
                del self.cols[table]
        for pair in self.connected.keys():
            if pair[0] in self.tables and pair[1] in self.tables:
                continue
            del self.connected[pair]
        for table in self.tables:
            self.cols[table] = postdata.get('%s_columns%s' % (table, widget_id))

    def __repr__(self):
        return str((self.target_table, self.tables, self.cols, self.connected))

