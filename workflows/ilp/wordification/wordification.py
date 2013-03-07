from collections import defaultdict
import string
class Wordification(object):

    def __init__(self,target_table,other_tables,context):
        """
        Wordification object constructor.
        
        @param target_table Orange ExampleTable, representing the primary table
        @param other_tables secondary tables, Orange ExampleTables
        """
        self.target_table=target_table
        self.other_tables=other_tables
        self.context=context

        self.connecting_tables=defaultdict(list)

        #finds table connections
        for primary_table in [target_table]+other_tables:
            for secondary_table in [target_table]+other_tables:
                if (primary_table.name,secondary_table.name) in self.context.connected:
                    primary_key,foreign_key=self.context.connected[(primary_table.name,secondary_table.name)]
                    if self.context.pkeys[primary_table.name] == primary_key:
                        self.connecting_tables[primary_table].append((secondary_table,foreign_key))


    def wordify(self):
        """
        Applies the wordification methodology on the target table
        """
        return string.join(["!"+str(ex.get_class())+" "+string.join(self.wordify_example(self.target_table,ex)," ") for ex in self.target_table],"\n")


    def wordify_example(self,data,ex):
        """
        Recursively constructs the 'wordification' document for the given example.

        @param data The given examples ExampleTable
        @param ex Example for which the document is constructed
        """

        words=[] #word list for every example
        ex_pkey_value=data.name in self.context.pkeys and ex[str(self.context.pkeys[data.name])]

        #Construct words (tableName_attributeName_attributeValue) from the given table
        for att in data.domain.attributes:
            if not str(att.name) in self.context.pkeys[data.name] and not str(att.name) in self.context.fkeys[data.name]:
                words.append(self.att_to_s(data.name)+"_"+self.att_to_s(att.name)+"_"+self.att_to_s(ex[att]))

        #Apply the wordification methodology recursively on all connecting tables
        for sec_t,sec_fkey in self.connecting_tables[data]:
            for sec_ex in sec_t:
                if ex_pkey_value and sec_ex[str(sec_fkey)]==ex_pkey_value:
                    words+=self.wordify_example(sec_t,sec_ex)
        return words

    def att_to_s(self,att):
        """
        Constructs a "wordification" word for the given attribute

        @param att Orange attribute
        """

        return str(att).title().replace(' ','').replace('_','')
