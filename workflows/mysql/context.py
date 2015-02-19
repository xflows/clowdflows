from collections import defaultdict
import pprint
import copy

from django import forms
import mysql.connector as sql
import converters


class DBConnection:
    '''
    Database credentials.
    '''
    def __init__(self, user, password, host, database):
        self.user = user
        self.password = password
        self.host = host
        self.database = database
        self.check_connection()

    def check_connection(self):
        try:
            con = sql.connect(user=self.user, password=self.password, host=self.host, database=self.database)
            con.close()
        except Exception,e:
            raise Exception('Problem connecting to the database. Please re-check your credentials.')

    def connect(self):
        return sql.connect(user=self.user, password=self.password, host=self.host, database=self.database)


class DBContext:
    def __init__(self, connection, find_connections=False, in_memory=True):
        '''
        Initializes the fields:
            tables:           list of selected tables
            cols:             dict of columns for each table
            all_cols:         dict of columns for each table (even unselected)
            col_vals:         available values for each table/column 
            connected:        dict of table pairs and the connected columns
            fkeys:            foreign keys in a given table
            reverse_fkeys:    fkey to table map
            pkeys:            private key for a given table
            target_table:     selected table for learning
            target_att:       selected column for learning
        '''
        self.connection = connection
        con = connection.connect()
        cursor = con.cursor()
        cursor.execute('SHOW tables')
        self.tables = [table for (table,) in cursor]
        self.cols = {}
        for table in self.tables:
            cursor.execute("SELECT column_name FROM information_schema.columns WHERE table_name = '%s' AND table_schema='%s'" % (table,connection.database))
            self.cols[table] = [col for (col,) in cursor]
        self.all_cols = dict(self.cols)
        self.col_vals = {}

        self.connected = defaultdict(list)
        cursor.execute(
           "SELECT table_name, column_name, referenced_table_name, referenced_column_name \
            FROM information_schema.KEY_COLUMN_USAGE \
            WHERE referenced_table_name IS NOT NULL AND table_schema='%s'" % connection.database)
        self.fkeys = defaultdict(set)
        self.reverse_fkeys = {}
        self.pkeys = {}
        if find_connections:
            for table in self.tables:
                for col in self.cols[table]:
                    if col.endswith('_id'):
                        ref_table = (col[:-4] + 'ies') if col[-4] == 'y' and col[-5] != 'e' else (col[:-3] + 's')
                        if ref_table in self.tables:
                            self.connected[(table, ref_table)].append((col, 'id'))
                            self.connected[(ref_table, table)].append(('id', col))
                            self.fkeys[table].add(col)
                            self.reverse_fkeys[(table, col)] = ref_table

                    if col == 'id':
                        self.pkeys[table] = col
        for (table, col, ref_table, ref_col) in cursor:
            self.connected[(table, ref_table)].append((col, ref_col))
            self.connected[(ref_table, table)].append((ref_col, col))
            self.fkeys[table].add(col)
            self.reverse_fkeys[(table, col)] = ref_table

        cursor.execute(
            "SELECT table_name, column_name \
             FROM information_schema.KEY_COLUMN_USAGE \
             WHERE constraint_name='PRIMARY' AND table_schema='%s'" % connection.database)
        for (table, pk) in cursor:
            self.pkeys[table] = pk
        self.target_table = self.tables[0]
        self.target_att = None
        con.close()

        self.orng_tables = None
        self.in_memory = in_memory

    def read_into_orange(self):
        conv = converters.Orange_Converter(self)
        tables = {
            self.target_table: conv.target_Orange_table()
        }
        other_tbl_names = [table for table in self.tables if table != self.target_table]
        other_tables = dict(zip(other_tbl_names, conv.other_Orange_tables()))
        tables.update(other_tables)
        return tables

    def update(self, postdata):
        '''
        Updates the default selections with user's selections.
        '''
        widget_id = postdata.get('widget_id')[0]
        self.target_table = postdata.get('target_table%s' % widget_id)[0]
        self.target_att = postdata.get('target_att%s' % widget_id)[0]
        #self.target_att_val = postdata.get('target_att_val%s' % widget_id)[0]
        self.tables = postdata.get('tables%s' % widget_id, [])
        if self.target_table not in self.tables:
            raise Exception('The selected target table "%s" is not among the selected tables.' % self.target_table)
        # Propagate the selected tables
        for table in self.cols.keys():
            if table not in self.tables:
                del self.cols[table]
        for pair in self.connected.keys():
            if pair[0] in self.tables and pair[1] in self.tables:
                continue
            del self.connected[pair]
        for table in self.tables:
            self.cols[table] = postdata.get('%s_columns%s' % (table, widget_id), [])
            if table == self.target_table and self.target_att not in self.cols[table]:
                raise Exception('The selected target attribute ("%s") is not among the columns selected for the target table ("%s").' % (self.target_att, self.target_table))
        if self.in_memory:
            self.orng_tables = self.read_into_orange()

    def fmt_cols(self, cols):
        return ','.join(["`%s`" % col for col in cols])

    def fetch(self, table, cols):
        '''
        Fetches rows from the db.
        '''
        con = self.connection.connect()
        cursor = con.cursor() 
        print "SELECT %s FROM %s" % (self.fmt_cols(cols), table)
        cursor.execute("SELECT %s FROM %s" % (self.fmt_cols(cols), table))
        result = [cols for cols in cursor]
        con.close()
        return result

    def rows(self, table, cols):
        '''
        Fetches rows from the local cache or from the db if there's no cache.
        '''
        if self.orng_tables:
            data = []
            for ex in self.orng_tables[table]:
                data.append([ex[str(col)] for col in cols])
            return data
        else:
            return self.fetch(table, cols)

    def select_where(self, table, cols, pk_att, pk):
        '''
        SELECT with WHERE clause.
        '''
        if self.orng_tables:
            data = []
            for ex in self.orng_tables[table]:
                if str(ex[str(pk_att)]) == str(pk):
                    data.append([ex[str(col)] for col in cols])
            return data
        else:
            con = self.connection.connect()
            cursor = con.cursor() 
            attributes = self.db.fmt_cols(cols)
            print "SELECT %s FROM %s WHERE `%s`='%s'" % (attributes, table, pk_att, pk)
            cursor.execute("SELECT %s FROM %s WHERE `%s`='%s'" % (attributes, table, pk_att, pk))
            result = [cols for cols in cursor]
            con.close()
            return results

    def fetch_types(self, table, cols):
        '''
        Returns a dictionary of field types for the given table and columns.
        '''
        con = self.connection.connect()
        cursor = con.cursor() 
        cursor.execute("SELECT %s FROM `%s` LIMIT 1" % (self.fmt_cols(cols), table))
        cursor.fetchall()
        types = {}
        for desc in cursor.description:
            types[desc[0]] = sql.FieldType.get_info(desc[1])
        con.close()
        return types

    def compute_col_vals(self):
        import time
        con = self.connection.connect()
        cursor = con.cursor()
        for table, cols in self.cols.items():
            self.col_vals[table] = {}
            for col in cols:
                cursor.execute("SELECT DISTINCT BINARY `%s`, `%s` FROM `%s`" % (col, col, table))
                self.col_vals[table][col] = [val for (_,val) in cursor]
        con.close()

    def copy(self):
        return copy.deepcopy(self)

    def __repr__(self):
        return pprint.pformat({
            'target_table' : self.target_table, 
            'target attribute' : self.target_att, 
            'tables' : self.tables, 
            'cols' : self.cols, 
            'connected' : self.connected, 
            'pkeys' : self.pkeys, 
            'fkeys' : self.fkeys,
            'orng_tables': [(name, len(table)) for name, table in self.orng_tables.items()] if self.orng_tables else 'not in memory'
        })

