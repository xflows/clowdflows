DEBUG = True
from collections import defaultdict
import string
class Wordification(object):
    # The aleph source file is presumed to be in the same dir as this file.
    #THIS_DIR = os.path.dirname(__file__) if os.path.dirname(__file__) else '.'


    ESSENTIAL_PARAMS = {
        'depth' : 10
    }

    def __init__(self,target_table,other_tables,context):
        """
        TODO:
        Creates an Aleph object.
        
        @param logging can be DEBUG, INFO or NOTSET (default). This controls the verbosity of the output.
        """
        self.target_table=target_table
        self.other_tables=other_tables
        self.context=context

        self.connecting_tables=defaultdict(list)

        for primary_table in [target_table]+other_tables:
            for secondary_table in [target_table]+other_tables:
                if (primary_table.name,secondary_table.name) in self.context.connected:
                    primary_key,foreign_key=self.context.connected[(primary_table.name,secondary_table.name)]
                    if self.context.pkeys[primary_table.name] == primary_key:
                        self.connecting_tables[primary_table].append((secondary_table,foreign_key))




    def wordify(self):
        """
        TODO dokumentacija:
        Induce a theory in 'mode'.
        
        @param filestem The base name of this experiment.
        @param mode In which mode to induce rules.
        @param pos String of positive examples.
        @param neg String of negative examples.
        @param b String with background knowledge.
        """
        return [string.join(self.wordify_example(self.target_table,ex)," ") for ex in self.target_table]


    def wordify_example(self,data,ex):
        #self.connected[(table, ref_table)] = (col, 'id')
        #self.connected[(ref_table, table)] = ('id', col)
        words=[] #word list for every example
        print data.name in self.context.pkeys and str(self.context.pkeys[data.name])
        ex_pkey_value=data.name in self.context.pkeys and ex[str(self.context.pkeys[data.name])]
        #for ex in data:
        #    if data==self.target_table or
        for att in data.domain.attributes:
            if not str(att) in self.context.pkeys[data.name] and not str(att) in self.context.fkeys[data.name]:
                words.append(self.att_to_s(data.name)+"_"+self.att_to_s(att.name)+"_"+self.att_to_s(ex[att]))

        for sec_t,sec_fkey in self.connecting_tables[data]:
            print sec_t
            print sec_fkey

            for sec_ex in sec_t:
                if ex_pkey_value and sec_ex[str(sec_fkey)]==ex_pkey_value:
                    words+=self.wordify_example(sec_t,sec_ex)
        return words

    def att_to_s(self,att):
        return str(att).title().replace(' ','').replace('_','')

#
# import os
# if os.getenv('COMPUTERNAME')=="PORFAVOR-PC":
#     db=_mysql.connect(host="localhost",user="root",
#                       passwd="pwd",db="imdb")
# else:
#     db=_mysql.connect(host="127.0.0.1",user="anze",
#                       passwd="123",db="imdb")
#
# trains_text=defaultdict(list)
# trains_class={}
#
# top250=[]
# bottom100=[]
# topfile = open("top250.txt", 'r') # open file for reading
# bottomfile = open("bottom100.txt","r") # open file for appending
#
# line = topfile.readline()    # Invokes readline() method on file
# i=0
# while line:
#     if line!="\n":
#         spl=line.split(" (")
#         #print "a",spl[1],"a",spl[1][0:4]
#         top250.append([spl[0],spl[1][0:4]])
#         #outfile.write("example"+str(i)+"\t"+line),    # trailing ',' omits newline character
#     line = topfile.readline()
# print top250
# topfile.close()
#
#
# line = bottomfile.readline()    # Invokes readline() method on file
# i=0
# while line:
#     if line!="\n":
#         spl=line.split(" (")
#         #print "a",spl[1],"a",spl[1][0:4]
#         bottom100.append([spl[0],spl[1][0:4]])
#         #outfile.write("example"+str(i)+"\t"+line),    # trailing ',' omits newline character
#     line = bottomfile.readline()
# print bottom100
# topfile.close()
#
# #class Movies(Storm):
# #    has_many('authors')
# #Storm.conn(user='root',db='imdb',passwd="pwd")
# #print Movies.select()
# #top250=["The Shawshank Redemption","The Godfather","The Godfather: Part II","Pulp Fiction","The Good, the Bad and the Ugly","12 Angry Men","Schindler's List","The Dark Knight","The Lord of the Rings: The Return of the King","One Flew Over the Cuckoo's Nest","Star Wars: Episode V - The Empire Strikes Back","Fight Club","Seven Samurai","Inception","The Lord of the Rings: The Fellowship of the Ring","Goodfellas","Star Wars: Episode IV - A New Hope","City of God","Casablanca","The Matrix","Once Upon a Time in the West","Rear Window","Raiders of the Lost Ark","The Silence of the Lambs","The Usual Suspects","The Lord of the Rings: The Two Towers","Se7en","Forrest Gump","The Avengers","Psycho","It's a Wonderful Life","Leon: The Professional","Sunset Blvd.","Memento","Dr. Strangelove or: How I Learned to Stop Worrying and Love the Bomb","Apocalypse Now","American History X","North by Northwest","Terminator 2: Judgment Day","Citizen Kane","Saving Private Ryan","Alien","City Lights","American Beauty","Spirited Away","Toy Story 3","Taxi Driver","The Shining","Vertigo","M","Paths of Glory","The Pianist","Modern Times","The Departed","Amelie","Double Indemnity","WALLE","The Lives of Others","Aliens","A Clockwork Orange","Life Is Beautiful","Back to the Future","To Kill a Mockingbird","Lawrence of Arabia","Das Boot","Requiem for a Dream","Reservoir Dogs","Eternal Sunshine of the Spotless Mind","The Third Man","A Separation","The Prestige","The Green Mile","Cinema Paradiso","Chinatown","The Treasure of the Sierra Madre","L.A. Confidential","The Great Dictator","Gladiator","Once Upon a Time in America","Monty Python and the Holy Grail","Rashomon","Full Metal Jacket","Bicycle Thieves","Amadeus","Some Like It Hot","Singin' in the Rain","All About Eve","Raging Bull","Metropolis","Braveheart","Oldboy","2001: A Space Odyssey","The Bridge on the River Kwai","The Apartment","Star Wars: Episode VI - Return of the Jedi","Pan's Labyrinth","Unforgiven","Princess Mononoke","The Sting","Downfall","The Intouchables","Indiana Jones and the Last Crusade","The Lion King","Inglourious Basterds","Mr. Smith Goes to Washington","Die Hard","Grave of the Fireflies","The Seventh Seal","On the Waterfront","Up","The Elephant Man","The Great Escape","The Maltese Falcon","Yojimbo","Gran Torino","Rebecca","Batman Begins","Witness for the Prosecution","For a Few Dollars More","Snatch.","The General","Heat","Blade Runner","Fargo","Ran","Wild Strawberries","Sin City","Toy Story","Touch of Evil","The Big Lebowski","The Deer Hunter","Jaws","Hotel Rwanda","No Country for Old Men","Ikiru","Cool Hand Luke","The Artist","It Happened One Night","Scarface","The Wizard of Oz","The Sixth Sense","The King's Speech","Strangers on a Train","Black Swan","The Kid","The Wages of Fear","Kill Bill: Vol. 1","Annie Hall","The Gold Rush","High Noon","Platoon","Warrior","Trainspotting","Butch Cassidy and the Sundance Kid","Lock, Stock and Two Smoking Barrels","Into the Wild","Sunrise","The Grapes of Wrath","The Secret in Their Eyes","Donnie Darko","The Thing","Notorious","Gone with the Wind","Casino","Million Dollar Baby","There Will Be Blood","Diabolique","Life of Brian","My Neighbor Totoro","Amores Perros","Finding Nemo","Ben-Hur","Slumdog Millionaire","How to Train Your Dragon","Groundhog Day","The Terminator","The Big Sleep","V for Vendetta","Good Will Hunting","The Best Years of Our Lives","Stand by Me","The Graduate","Dog Day Afternoon","Judgment at Nuremberg","Twelve Monkeys","Network","The Bourne Ultimatum","The Manchurian Candidate","The 400 Blows","The Night of the Hunter","Mary and Max","Harakiri","Gandhi","District 9","Persona","Dial M for Murder","The Battle of Algiers","The Princess Bride","The Killing","81","La strada","Who's Afraid of Virginia Woolf?","The Hustler","The Passion of Joan of Arc","Howl's Moving Castle","Sherlock Jr.","Ratatouille","The Wrestler","The Exorcist","Fanny and Alexander","The Wild Bunch","The Diving Bell and the Butterfly","Kind Hearts and Coronets","Harry Potter and the Deathly Hallows: Part 2","Rocky","Stalag 17","Barry Lyndon","A Streetcar Named Desire","Nights of Cabiria","Star Trek","All Quiet on the Western Front","A Beautiful Mind","The Truman Show","Infernal Affairs","Roman Holiday","Rope","Tokyo Story","Ip Man","The Man Who Shot Liberty Valance","Come and See","High and Low","Stalker","Rosemary's Baby","The Celebration","Mystic River","Let the Right One In","Pirates of the Caribbean: The Curse of the Black Pearl","Nausicaa of the Valley of the Wind","Beauty and the Beast","Monsters, Inc.","Manhattan","Anatomy of a Murder","Nosferatu","Le Samourai","Magnolia","Throne of Blood","Big Fish","In the Mood for Love","Grand Illusion","La Haine"]
# #bottom100=["Titanic: The Legend Goes On...","Superbabies: Baby Geniuses 2","Daniel the Wizard","Monster a-Go Go","Manos: The Hands of Fate","Night Train to Mundo Fine","Dream Well","Ben & Arthur","The Skydivers","The Starfighters","Zombie Nation","Pledge This!","Too Beautiful","The Little Fox 2","Fat Slags","From Justin to Kelly","The Hillz","Zaat","Ram Gopal Varma's Indian Flames","LOL","The Final Sacrifice","Turks in Space","Merlin's Shop of Mystical Wonders","The Beast of Yucca Flats","A Story About Love","Disaster Movie","The Apocalypse","Hobgoblins","The Pod People","Shark","Track of the Moon Beast","The Creeping Terror","Who's Your Caddy?","The Wild World of Batwoman","Zodiac Killer","Girl in Gold Boots","Crossover","Space Mutiny","Yes Sir","The Tony Blair Witch Project","The Hottie & the Nottie","Going Overboard","The Maize: The Movie","The Barbaric Beast of Boggy Creek, Part II","Birdemic: Shock and Terror","The Blade Master","Keloglan vs. the Black Prince","Die Hard Dracula","Prince of Space","The Pumaman","Glitter","Surf School","Zombie Nightmare","House of the Dead","Soultaker","Danes Without a Clue","Anne B. Real","Son of the Mask","Eegah","Tangents","Popstar","Chairman of the Board","Leonard Part 6","The Incredibly Strange Creatures Who Stopped Living and Became Mixed-Up Zombies!!?","Santa Claus","Miss Castaway and the Island Girls","Santa with Muscles","Car 54, Where Are You?","Snowboard Academy","Epic Movie","Anus Magillicutty","Lawnmower Man 2: Beyond Cyberspace","Baby Geniuses","Feel the Noise","Nine Lives","Mitchell","Demon Island","Alone in the Dark","Seven Mummies","Santa Claus Conquers the Martians","Cool as Ice","Laserblast","Simon Sez","Body in the Web","3 Ninjas: High Noon at Mega Mountain","In the Mix","Ed","Barney's Great Adventure","Troll 2","Battlefield Earth: A Saga of the Year 3000","Gigli","Time You Change","The Smokers","American Ninja V","It's Pat","Phat Girlz","Addiction","Another Nine & a Half Weeks","Meet the Spartans","Alien from L.A."]
# #QUERY MOVIES
# db.query("SELECT * FROM movies WHERE "+string.join(["name=\""+a[0]+"\" AND year="+a[1] for a in top250+bottom100], ' OR '))
# #print "SELECT * FROM movies WHERE name IN (\""+string.join(bottom100, '\",\"')+"\")"
# r=db.store_result()
# movies={}
# for idd,title,year,rank in r.fetch_row(maxrows=0):
#     if int(idd)%1==0:
#         movies[idd]={"title":"title_"+title,"year":year,"rank":rank}
#         trains_text[idd].append(re.sub("[^\w]","","title_"+title))
#         #print year,"age_old_movie" if int(year)<1950 else "age_fairly_new_movie" if int(year)<2000 else "age_new_movie"
#         trains_text[idd].append("age_old_movie" if int(year)<1950 else "age_fairly_new_movie" if int(year)<2000 else "age_new_movie")
#         trains_class[idd]="GOOD" if not [title,year] in bottom100 else "BAD"
#
# print trains_class.values().count("GOOD"),trains_class.values().count("BAD")
# #QUERY ACTORS
# db.query("""SELECT * FROM actors""")
#
# cnt=0
#
# r=db.store_result()
# actors={}
# for idd,first_name,last_name,gender in r.fetch_row(maxrows=0):
#     actors[idd]={"first_name":first_name,"last_name":last_name,"gender":gender}
#
#
#
# #roles
# relevant_actors=set([])
# cnt=0
# db.query("""SELECT * FROM roles""")
# r=db.store_result()
# for actor_id,movie_id,role_name in r.fetch_row(maxrows=0):
#     #authors[idd]={"first_name":first_name,"last_name":last_name,"gender":gender}
#     if trains_text.has_key(movie_id):
#         cnt+=1
#         relevant_actors.add(actor_id)
#         trains_text[movie_id].append("actor_"+re.sub("[^\w]","",actors[actor_id]["first_name"])+re.sub("[^\w]","",actors[actor_id]["last_name"]))
# print "roles",cnt
# print "actors",len(relevant_actors)
# cnt=0
#
# #append genres
# db.query("""SELECT * FROM movies_genres""")
# r=db.store_result()
# for movie_id,genre in r.fetch_row(maxrows=0):
#     #authors[idd]={"first_name":first_name,"last_name":last_name,"gender":gender}
#     if trains_text.has_key(movie_id):
#         cnt+=1
#         trains_text[movie_id].append("movieGenre_"+genre)
#         #trains_text[movie_id].append(genre+"_movie")
# print "movie_genres",cnt
# cnt=0
#
#
#
#
# #QUERY DIRECTORS
# db.query("""SELECT * FROM directors""")
#
# r=db.store_result()
# directors={}
# for idd,first_name,last_name in r.fetch_row(maxrows=0):
#     directors[idd]={"first_name":first_name,"last_name":last_name}
#
#
# #QUERY DIRECTORS
# db.query("""SELECT * FROM directors_genres""")
# r=db.store_result()
# director_genres=defaultdict(list)
# for director_id,genre,prob in r.fetch_row(maxrows=0):
#     if float(prob)>=0.5:
#         #cnt+=1
#         director_genres[director_id].append(re.sub("[^\w]","","directorGenre_"+genre))
# #print "director_genres",cnt
#
#
# #QUERY DIRECTORS
# relevant_directors=set([])
# cnt=0
# cnt2=0
# db.query("""SELECT * FROM movies_directors""")
# r=db.store_result()
# for director_id,movie_id in r.fetch_row(maxrows=0):
#     #director_genres[director_id].append(re.sub("[^\w]","","genre_"+genre))
#     if directors.has_key(director_id) and movies.has_key(movie_id):
#         cnt+=1
#         cnt2+=len(director_genres[director_id])
#         relevant_directors.add(director_id)
#         trains_text[movie_id].append(re.sub("[^\w]","","director_"+directors[director_id]["first_name"]+directors[director_id]["last_name"]))
#         trains_text[movie_id].extend(director_genres[director_id])
# print "movies_directos",cnt
# print "director_genres",cnt2
# print "directors",len(relevant_directors)
#
#
#
# ##QUERY DIRECTORS
# #db.query("""SELECT * FROM movies_genres""")
# #r=db.store_result()
# #for director_id,movie_id in r.fetch_row(maxrows=0):
# #
# #    if director_genres.has_key(director_id):
# #        for g in director_genres[director_id]:
# #            if trains_text.has_key(movie_id):
# #                trains_text[movie_id].append("movieGenre_"+g)#+"_directorGenre")
#
#
# print trains_text.items()[0:10]
#
# words = set()
# tf_idfs = {}
#
# for train in trains_text.keys():
#     for word in trains_text[train]:
#         words.add(word)
#
# word_count=defaultdict(int)
#
# for train_words in trains_text.values():
#     for word in set(train_words):
#         word_count[word]+=1
#
# #def count_trains(trains_text, word):
# #    print "ct"
# #
# #    if not word_count.has_key(word):
# #        cnt = 0
# #        for train, train_words in trains_text.items():
# #            if word in train_words:
# #                cnt = cnt + 1
# #        word_count[word]=cnt
# #    return word_count[word]
# len_train_text=len(trains_text)
# print "compute tf-idf"
# for train, train_words in trains_text.items():
#
#     print str(train)
#     train_word_count=defaultdict(int)
#     tf_idfs[train] = {}
#     for word in train_words:
#         train_word_count[word]+=1
#
#     for word,tf in train_word_count.items():
#         #tf = train_words.count(word)
#         #idf = log(len(trains_text.keys()) / float(count_trains(trains_text, word)))
#         idf = log(len_train_text / float(word_count[word]))
#
#         #print train, word, tf*idf
#         tf_idfs[train][word] = tf * idf
#
#
# binary = True
#
# out_file = open("imdb_bin.tab", "wb")
# #f = open('trains.tab','w')
# sorted_words = sorted(list(words))
# print "writting to file",len(sorted_words)
# for word in sorted_words:
#     out_file.write( word + "\t")
# out_file.write( "rating\n")
#
# for _ in range(len(sorted_words)):
#     out_file.write( "d\t")
# out_file.write( "d\n")
# nums=0
# tfnnull=0
# max_tfidf=max([item for t in tf_idfs.values() for item in t.values()])
# print max_tfidf
# for _ in range(len(sorted_words)):
#     out_file.write( "\t")
# out_file.write( "class\n")
#
#
# for train in trains_class.keys():
#     #if train%500==0:
#     for word in sorted_words:
#         if binary:
#             num = 1 if word in trains_text[train] else 0
#         else:
#             #num=tf_idfs[train].get(word, 0.0)
#             if tf_idfs[train].get(word, 0.0)!=0:
#                 tfnnull+=1
#             num=1 if tf_idfs[train].get(word, 0.0)/max_tfidf >0.3 else "?"
#             #nums+=num
#         out_file.write( str(num) + "\t")
#     out_file.write( trains_class[train]+"\n")
#
# print len(trains_class), str(nums)+"/"+str(tfnnull)