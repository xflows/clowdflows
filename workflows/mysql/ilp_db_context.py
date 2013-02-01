'''
Classes for handling DBContexts for ILP systems.

@author: Anze Vavpetic <anze.vavpetic@ijs.si>
'''

class ILP_DBContext:
    '''
    Base class for converting between a given database context (selected tables, columns, etc)
    to inputs acceptable by a specific ILP system.

    If possible, all subclasses should use lazy selects by forwarding the DB connection.
    '''
    def __init__(self, dbcontext, settings={}):
        self.db = dbcontext
        self.connection = dbcontext.connection.connect()
        self.cursor = self.connection.cursor()
        self.settings = settings

    def __del__(self):
        self.connection.close()

    def rows(self, table, cols):
        self.cursor.execute("SELECT %s FROM %s" % (','.join(cols), table))
        return [cols for cols in self.cursor]

    def user_settings(self):
        return [':- set(%s,%s).' % (key,val) for key, val in self.settings.items()]

    def mode(self, predicate, args, recall=1, head=False):
        return ':- mode%s(%s, %s(%s)).' % ('h' if head else 'b', str(recall), predicate, ','.join([t+arg for t,arg in args]))

    def db_connection(self):
        con = self.db.connection
        host, db, user, pwd = con.host, con.database, con.user, con.password
        return [':- use_module(library(myddas)).', \
                ':- db_open(mysql, \'%s\'/\'%s\', \'%s\', \'%s\').' % (host, db, user, pwd)] + \
               [':- db_import(%s, %s).' % (table, table) for table in self.db.tables]

    def connecting_clause(self, table, ref_table):
        var_table, var_ref_table = table.capitalize(), ref_table.capitalize()
        pk, fk = self.db.connected[(table, ref_table)]
        ref_pk = self.db.pkeys[ref_table]
        table_args, ref_table_args = [], []
        for col in self.db.cols[table]:
            if col == pk:
                col = var_table
            elif col == fk:
                col = var_ref_table
            table_args.append(col.capitalize())
        for col in self.db.cols[ref_table]:
            if col == ref_pk:
                col = var_ref_table
            if col == fk:
                col = var_table
            ref_table_args.append(col.capitalize())
        return ['has_%s(%s, %s) :-' % (ref_table, var_table.capitalize(), var_ref_table.capitalize()),
                '\t%s(%s),' % (table, ','.join(table_args)),
                '\t%s(%s).' % (ref_table, ','.join(ref_table_args))]

    def attribute_clause(self, table, att):
        var_table, var_att, pk = table.capitalize(), att.capitalize(), self.db.pkeys[table]
        return ['%s_%s(%s, %s) :-' % (table, att, var_table, var_att),
                '\t%s(%s).' % (table, ','.join([att.capitalize() if att!=pk else var_table for att in self.db.cols[table]]))]

class RSD_DBContext(ILP_DBContext):
    '''
    Converts the database context to RSD inputs.
    '''
    def all_examples(self):
        target = self.db.target_table
        examples = self.rows(target, [self.db.target_att, self.db.pkeys[target]])
        return '\n'.join(['%s(%s, %s).' % (target, cls, pk) for cls, pk in examples])

    def background_knowledge(self):
        modeslist, getters = [self.mode(self.db.target_table, [('+', self.db.target_table)], head=True)], []
        for (table, ref_table) in self.db.connected.keys():
            if ref_table == self.db.target_table:
                continue # Skip backward connections
            modeslist.append(self.mode('has_%s' % ref_table, [('+', table), ('-', ref_table)]))
            getters.extend(self.connecting_clause(table, ref_table))
        for table, atts in self.db.cols.items():
            for att in atts:
                if att == self.db.target_att and table == self.db.target_table or \
                   att in self.db.fkeys[table] or att == self.db.pkeys[table]:
                    continue
                modeslist.append(self.mode('%s_%s' % (table, att), [('+', table), ('-', att)]))
                modeslist.append(self.mode('instantiate', [('+', att)]))
                getters.extend(self.attribute_clause(table, att))
        return '\n'.join(self.db_connection() + modeslist + getters + self.user_settings())

class Aleph_DBContext(ILP_DBContext):
    '''
    Converts the database context to Aleph inputs.
    '''
    def __init__(self, *args, **kwargs):
        ILP_DBContext.__init__(self, *args, **kwargs)
        self.__pos_examples, self.__neg_examples = None, None

    def __examples(self):
        if not (self.__pos_examples and self.__neg_examples):
            target, att, target_val = self.db.target_table, self.db.target_att, self.db.target_att_val
            rows = self.rows(target, [att, self.db.pkeys[target]])
            pos_rows, neg_rows = [], []
            for row in rows:
                if row[0] == target_val:
                    pos_rows.append(row)
                else:
                    neg_rows.append(row)
            self.__pos_examples = '\n'.join(['%s(%s).' % (target_val, id) for _, id in pos_rows])
            self.__neg_examples = '\n'.join(['%s(%s).' % (target_val, id) for _, id in neg_rows])
        return self.__pos_examples, self.__neg_examples

    def positive_examples(self):
        return self.__examples()[0]

    def negative_examples(self):
        return self.__examples()[1]

    def background_knowledge(self):
        modeslist, getters = [self.mode(self.db.target_att_val, [('+', self.db.target_table)], head=True)], []
        determinations, types = [], []
        for (table, ref_table) in self.db.connected.keys():
            if ref_table == self.db.target_table:
                continue # Skip backward connections
            modeslist.append(self.mode('has_%s' % ref_table, [('+', table), ('-', ref_table)], recall='*'))
            determinations.append(':- determination(%s/1, has_%s/2).' % (self.db.target_att_val, ref_table))
            types.extend(self.concept_type_def(table))
            types.extend(self.concept_type_def(ref_table))
            getters.extend(self.connecting_clause(table, ref_table))
        for table, atts in self.db.cols.items():
            for att in atts:
                if att == self.db.target_att and table == self.db.target_table or \
                   att in self.db.fkeys[table] or att == self.db.pkeys[table]:
                    continue
                modeslist.append(self.mode('%s_%s' % (table, att), [('+', table), ('#', att)], recall='*'))
                determinations.append(':- determination(%s/1, %s_%s/2).' % (self.db.target_att_val, table, att))
                types.extend(self.constant_type_def(table, att))
                getters.extend(self.attribute_clause(table, att))
        local_copies = [self.local_copy(table) for table in self.db.tables]
        return '\n'.join(self.db_connection() + local_copies + self.user_settings() + modeslist + determinations + types + getters)

    def concept_type_def(self, table):
        #return ['%s(%s).' % (table, id) for (id,) in self.rows(table, [self.db.pkeys[table]])]
        var_pk = self.db.pkeys[table].capitalize()
        variables = ','.join([var_pk if col.capitalize() == var_pk else '_' for col in self.db.cols[table]])
        return ['%s(%s) :-' % (table, var_pk), 
                '\t%s(%s).' % (table, variables)]

    def constant_type_def(self, table, att):
        # return ['%s(%s).' % (att, val) for val in self.db.col_vals[att]]
        var_att = att.capitalize()
        variables = ','.join([var_att if col == att else '_' for col in self.db.cols[table]])
        return ['%s(%s) :-' % (att, var_att), 
                '\t%s(%s).' % (table, variables)]

    def db_connection(self):
        con = self.db.connection
        host, db, user, pwd = con.host, con.database, con.user, con.password
        return [':- use_module(library(myddas)).', \
                ':- db_open(mysql, \'%s\'/\'%s\', \'%s\', \'%s\').' % (host, db, user, pwd)] + \
               [':- db_import(%s, tmp_%s).' % (table, table) for table in self.db.tables]

    def local_copy(self, table):
        cols = ','.join([col.capitalize() for col in self.db.cols[table]])
        return ':- repeat, tmp_%s(%s), (%s(%s), !, fail ; assertz(%s(%s)), fail).' % (table, cols, table, cols, table, cols)

if __name__ == '__main__':
    from context import DBConnection, DBContext

    context = DBContext(DBConnection('root','','localhost','test'))
    context.target_table = 'trains'
    context.target_att = 'direction'
    context.target_att_val = 'east'

    rsd = RSD_DBContext(context)
    ex, bk = rsd.all_examples(), rsd.background_knowledge()

    aleph = Aleph_DBContext(context)
    print aleph.positive_examples()
    print aleph.negative_examples()
    print aleph.background_knowledge()