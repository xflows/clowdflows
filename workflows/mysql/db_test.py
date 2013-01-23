import mysql.connector as sql

# Widget 1
con = sql.connect(user='root', password='', host='localhost', database='test')

# Widget 2
# Tole bo DB Context Object
# - izberi relacije
# - izberi stolpce
# - povezi kljuce
cursor = con.cursor()
cursor.execute('SHOW tables')
tables = [table for (table,) in cursor]
cols = {}
for table in tables:
    cursor.execute("SELECT column_name FROM information_schema.columns WHERE table_name = '%s'" % table)
    cols[table] = [col for (col,) in cursor]
print cols
main_table = 'trains'
connected = {}
cursor.execute(
   "SELECT table_name, column_name, referenced_table_name, referenced_column_name \
    FROM information_schema.KEY_COLUMN_USAGE \
    WHERE referenced_table_name IS NOT NULL")
for (table, col, ref_table, ref_col) in cursor:
    connected[(table, ref_table)] = (col, ref_col)
print connected

# Widget 3 
# - iz DB context obj zgeneriraj proper .b in .f/.n fajle
