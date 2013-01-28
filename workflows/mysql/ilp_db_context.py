'''
Classes for handling DBContexts for ILP systems.

@author: Anze Vavpetic <anze.vavpetic@ijs.si>
'''

class ILP_DBContext:
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

class RSD_DBContext(ILP_DBContext):
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

    def mode(self, predicate, args, recall=1, head=False):
        return ':- mode%s(%d, %s(%s)).' % ('h' if head else 'b', recall, predicate, ','.join([t+arg for t,arg in args]))

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
        var_table, var_att = table.capitalize(), att.capitalize()
        return ['has_%s(%s, %s) :-' % (att, var_table, var_att),
                '\t%s(%s).' % (table, ','.join([att.capitalize() for att in self.db.cols[table]]))]

    def db_connection(self):
        con = self.db.connection
        host, db, user, pwd = con.host, con.database, con.user, con.password
        return [':- use_module(library(myddas)).', \
                ':- db_open(mysql, \'%s\'/\'%s\', \'%s\', \'%s\').' % (host, db, user, pwd)] + \
               [':- db_import(%s, %s).' % (table, table) for table in self.db.tables]

    def user_settings(self):
        return [':- set(%s,%s).' % (key,val) for key, val in self.settings.items()]

class Aleph_DBContext(ILP_DBContext):
    def positive_examples(self):
        pass
    def negative_examples(self):
        pass
    def background_knowledge(self):
        pass

from context import DBConnection, DBContext

context = DBContext(DBConnection('root','','localhost','test'))
context.target_att = 'shape'

rsd = RSD_DBContext(context)
print rsd.all_examples()
print rsd.background_knowledge()