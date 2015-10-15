from collections import defaultdict
import pprint
import copy

import converters
import psycopg2 as postgresql
import mysql.connector as mysql

class DBConnection:
    '''
    Database credentials.
    '''
    def __init__(self, user, password, host, database,dal):
        self.user = user
        self.password = password
        self.host = host
        self.database = database
        self.dal = dal

        self.check_connection()
            
    def check_connection(self):
        try:
            con = self.connect()
            con.close()
        except Exception:
            raise Exception('Problem connecting to the database. Please re-check your credentials.')

    def connect(self):
        return self.dal.connect(user=self.user, password=self.password, host=self.host, database=self.database)

class MySqlDAL:
    def __init__(self):
        return
    
    def connect(self, user, password, host, database):
        return mysql.connect(user=user, password=password,  host=host, database=database)

    def list_tables(self, cursor, database):
        cursor.execute('SHOW tables')

    def list_columns(self, cursor, table, database):
        cursor.execute("SELECT column_name FROM information_schema.columns WHERE table_name = '%s' AND table_schema='%s'" % (table,database))
   
    def list_foreign_keys(self, cursor, database):
        cursor.execute(
           "SELECT table_name, column_name, referenced_table_name, referenced_column_name \
            FROM information_schema.KEY_COLUMN_USAGE \
            WHERE referenced_table_name IS NOT NULL AND table_schema='%s'" % database)

    def list_table_column_name(self, cursor, database):
        cursor.execute(
            "SELECT table_name, column_name \
             FROM information_schema.KEY_COLUMN_USAGE \
             WHERE constraint_name='PRIMARY' AND table_schema='%s'" % database)
        
    def fmt_cols(self, cols):        
        return ','.join(["`%s`" % col for col in cols])
    
    def search_types(self, cursor,cols,table):
        types = {}
        cursor.execute("SELECT %s FROM `%s` LIMIT 1" % (self.fmt_cols(cols), table))
        cursor.fetchall()
        for desc in cursor.description:
                types[desc[0]] = mysql.FieldType.get_info(desc[1])
        return types
    
    def list_val_from_table(self,cursor,col,table):
        cursor.execute("SELECT DISTINCT `%s`, `%s` FROM `%s`" % (col,col, table))
        
    def list_val_from_table_where(self,cursor,attributes,table,att,val_att):
        cursor.execute("SELECT %s FROM %s WHERE `%s`='%s'" % (attributes, table, att, val_att))          
    
    def get_driver_name(self):
        return 'com.mysql.jdbc.Driver'

    def get_jdbc_prefix(self):
        return 'jdbc:mysql://'

class PgSqlDAL:
    def __init__(self):
        return
    
    def connect(self, user, password, host, database):
        return postgresql.connect(user=user, password=password,  host=host, database=database)
    
    def list_tables(self, cursor, database):
        cursor.execute("SELECT table_name FROM information_schema.tables WHERE table_schema=\'public\' \
                        AND table_type=\'BASE TABLE\' AND table_catalog='%s' AND table_name NOT LIKE \'\\_%%\'" % (database))

    def list_columns(self, cursor, table, database):
        cursor.execute("SELECT column_name FROM information_schema.columns \
        WHERE table_name = '%s' AND table_catalog='%s'" % (table,database))
   
    def list_foreign_keys(self, cursor, database):
        cursor.execute("SELECT \
                tc.table_name, kcu.column_name, \
                ccu.table_name AS referenced_table_name,\
                ccu.column_name AS referenced_column_name \
                FROM \
                information_schema.table_constraints AS tc \
                JOIN information_schema.key_column_usage AS kcu \
                  ON tc.constraint_name = kcu.constraint_name \
                JOIN information_schema.constraint_column_usage AS ccu \
                  ON ccu.constraint_name = tc.constraint_name \
                  WHERE constraint_type = 'FOREIGN KEY' AND tc.table_catalog='%s'" % database)
        
    def list_table_column_name(self, cursor, database):
        cursor.execute(
            "SELECT \
            tc.table_name, kcu.column_name \
            FROM \
            information_schema.table_constraints AS tc\
            JOIN information_schema.key_column_usage AS kcu \
            ON tc.constraint_name = kcu.constraint_name \
            WHERE constraint_type = 'PRIMARY KEY' AND tc.table_catalog='%s'" % database) 

    def fmt_cols(self, cols):
        return ','.join(["%s" % col for col in cols])
    
    def search_types(self, cursor, cols, table):
        types = {}
        cursor.execute("SELECT attname as col_name, atttypid::regtype AS base_type \
                    FROM pg_catalog.pg_attribute WHERE attrelid = 'public.%s'::regclass \
                    AND attnum > 0 AND NOT attisdropped ORDER  BY attnum;" % table)
        for rows in cursor:
            types[rows[0]] = rows[1]
        return types
    
    def list_val_from_table(self,cursor,col,table):
        cursor.execute("SELECT DISTINCT %s, %s FROM %s" % (col,col, table))
        
    def list_val_from_table_where(self,cursor,attributes,table,att,val_att):
        cursor.execute("SELECT %s FROM %s WHERE %s='%s'" % (attributes, table, att, val_att))
    
    def get_driver_name(self):
        return 'org.postgresql.Driver'

    def get_jdbc_prefix(self):
        return 'jdbc:postgresql://'       
        
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

        connection.dal.list_tables(cursor,connection.database)
           
        self.tables = [table for (table,) in cursor]
        self.cols = {}
        for table in self.tables:
            connection.dal.list_columns(cursor,table,connection.database)
                
            self.cols[table] = [col for (col,) in cursor]
        self.all_cols = dict(self.cols)
        self.col_vals = {}

        self.connected = defaultdict(list)
        connection.dal.list_foreign_keys(cursor,connection.database)
            
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
        
        connection.dal.list_table_column_name(cursor,connection.database)  

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
        return self.connection.dal.fmt_cols(cols)


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
            self.connection.dal.list_val_from_table_where(cursor,attributes,table,pk_att,pk)
            result = [cols for cols in cursor]
            con.close()
            return result

    def fetch_types(self, table, cols):
        '''
        Returns a dictionary of field types for the given table and columns.
        '''
        con = self.connection.connect()
        cursor = con.cursor() 
        types = self.connection.dal.search_types(cursor,cols,table)
        con.close()
        return types

    def compute_col_vals(self):
        import time
        con = self.connection.connect()
        cursor = con.cursor()
        for table, cols in self.cols.items():
            self.col_vals[table] = {}
            for col in cols:
                self.connection.dal.list_val_from_table(cursor,col,table)
                                      
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

    def change_table(self, new_table):
        target_att = self.target_att
        self.__init__(self.connection, find_connections='yes')
        con = self.connection.connect()
        cursor = con.cursor()
        self.target_table = new_table
        self.target_att = target_att
        self.tables = [new_table]
        self.cols = {}
        self.connection.dal.list_columns(cursor,new_table,self.connection.database)
        self.cols[new_table] = [col for (col,) in cursor]
    
        for pair in self.connected.keys():
            if pair[0] in self.tables and pair[1] in self.tables:
                continue
            del self.connected[pair]
        if self.in_memory:
            self.orng_tables = self.read_into_orange()
                
        con.close()
        return self
