from collections import defaultdict
import string
class Wordification(object):

    def __init__(self,target_table,other_tables,context,word_att_length):
        """
        Wordification object constructor.
        
        @param target_table Orange ExampleTable, representing the primary table
        @param other_tables secondary tables, Orange ExampleTables
        """
        self.target_table=target_table
        self.other_tables=other_tables
        self.context=context
        self.word_att_length=word_att_length

        self.connecting_tables=defaultdict(list)
        self.cached_sentences=defaultdict(dict)
        self.lll=defaultdict(int)


        #finds table connections
        for primary_table in [target_table]+other_tables:
            for secondary_table in [target_table]+other_tables:
                if (primary_table.name,secondary_table.name) in self.context.connected:
                    for primary_key,foreign_key in self.context.connected[(primary_table.name,secondary_table.name)]:
                        if self.context.pkeys[primary_table.name] == primary_key:
                            self.connecting_tables[primary_table].append((secondary_table,foreign_key,None))
                        #else:
                        #    self.connecting_tables[primary_table].append((secondary_table,primary_key,foreign_key))




        self.index_by_value={}
        for table in [target_table]+other_tables:
            self.index_by_value[table.name]={}
        for sec_t,sec_fkey,prim_fkey in [item for sublist in self.connecting_tables.values() for item in sublist]:
            #if sec_t==table:
            if not prim_fkey:
                self.index_by_value[sec_t.name][sec_fkey]=defaultdict(list)
                for ex in sec_t:
                    self.index_by_value[sec_t.name][sec_fkey][str(ex[str(sec_fkey)])].append(ex)
            else:
                if not prim_fkey in self.index_by_value[sec_t.name]:
                    self.index_by_value[sec_t.name][prim_fkey]=defaultdict(list)

                for ex in sec_t:
                    self.index_by_value[sec_t.name][prim_fkey][str(ex[str(prim_fkey)])].append(ex)

        print self.connecting_tables


    def wordify(self):
        """
        Applies the wordification methodology on the target table
        """

        #class + wordification on every example of the main table

        a=[]
        for i,ex in enumerate(self.target_table):
            a.append("!"+str(ex.get_class())+" "+string.join(self.wordify_example(self.target_table,ex)," "))

        s=string.join(a,"\n")
        #print sorted(self.lll.items(),key=lambda k: [k[1],k[0]],reverse=True)
        #print a
        #print "s"
        return s#[0:10000000]

    def wordify_example(self,data,ex,searched_connections=set([])):
        """
        Recursively constructs the 'wordification' document for the given example.

        @param data The given examples ExampleTable
        @param ex Example for which the document is constructed
        """
        debug=False
        data_name=str(data.name)
        if data_name=="ring_strucs":
            print data_name
        if debug:
            print "======================================"
            print "example:",ex
            print "table name:", data_name
            print "searched_connections:",len(searched_connections),searched_connections
            print "connecting_tables:",len(self.connecting_tables[data]),self.connecting_tables[data]

        ex_pkey_value=data.name in self.context.pkeys and ex[str(self.context.pkeys[data.name])]
        self.lll[data_name+" "+str(ex_pkey_value)]+=1

        if not data_name in self.cached_sentences or not str(ex_pkey_value) in self.cached_sentences[data.name]:
        #else:
            #print data_name,str(ex_pkey_value)
            words=[] #word list for every example
            if debug:
                print "words:",len(words)
            #Construct words (tableName_attributeName_attributeValue) from the given table
            for att in data.domain.attributes:
                if not str(att.name) in self.context.pkeys[data.name] and not str(att.name) in self.context.fkeys[data.name]:
                    words.append(self.att_to_s(data.name)+"_"+self.att_to_s(att.name)+"_"+self.att_to_s(ex[att]))

            #words from pairs of attributes
            single_words=words[:]
            if self.word_att_length>1:
                for i,att1 in enumerate(single_words):
                    for j,att2 in enumerate(single_words):
                        if i<j:
                            words.append(att1+"__"+att2)
                            #print "2",words[-1]

            if self.word_att_length>2:
                for i,att1 in enumerate(single_words):
                    for j,att2 in enumerate(single_words):
                        for k,att3 in enumerate(single_words):
                            if i<j and j<k:
                                words.append(att1+"__"+att2+"__"+att3)
                                #print "3",words[-1]


        #Apply the wordification methodology recursively on all connecting tables
            for sec_t,sec_fkey,prim_fkey in self.connecting_tables[data]:
                #for sec_ex in sec_t:
                #    if ex_pkey_value and sec_ex[str(sec_fkey)]==ex_pkey_value:
                #        words+=self.wordify_example(sec_t,sec_ex)
                #print sec_t,sec_fkey,prim_fkey
                if debug:
                    print "------------------"
                    print "(sec_t,sec_fkey,prim):",(sec_t.name,sec_fkey,prim_fkey)
                    print "search this table:",not (sec_t,sec_fkey) in searched_connections and sec_t!=self.target_table
                    print "search this table:",not prim_fkey or not (data,sec_fkey) in searched_connections# and sec_t!=self.target_table
                if not (sec_t,sec_fkey) in searched_connections and sec_t!=self.target_table and (not prim_fkey or not (data,sec_fkey) in searched_connections):
                    by_value=self.index_by_value[sec_t.name][str(sec_fkey)][str(ex_pkey_value)] if not prim_fkey else self.index_by_value[sec_t.name][str(prim_fkey)][str(ex[str(sec_fkey)])]

                    for sec_ex in by_value:
                        words+=self.wordify_example(sec_t,sec_ex,searched_connections | set([(sec_t,sec_fkey),prim_fkey and (data,prim_fkey)]))



            self.cached_sentences[data_name][str(ex_pkey_value)]=words

        return self.cached_sentences[data_name][str(ex_pkey_value)]

    def att_to_s(self,att):
        """
        Constructs a "wordification" word for the given attribute

        @param att Orange attribute
        """
        return str(att).title().replace(' ','').replace('_','')
