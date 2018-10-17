
# coding: utf-8

# In[50]:


#Importing the packages

import re
import pandas as pd 
import numpy as np 
import matplotlib.pyplot as plt 
import seaborn as sns
import string
import nltk
import warnings

warnings.filterwarnings("ignore", category=DeprecationWarning)

get_ipython().run_line_magic('matplotlib', 'inline')


# In[ ]:


import sys
get_ipython().system('{sys.executable} -m pip install wordcloud')


# In[5]:



#this is where you have to put your files
#C:\Users\Carlos7

#this is the tutorial:
#https://www.analyticsvidhya.com/blog/2018/07/hands-on-sentiment-analysis-dataset-python/


# In[76]:


train  = pd.read_csv('train_E6oV3lV.csv')
test = pd.read_csv('test_tweets_anuFYb8.csv')


# In[77]:


train.head()


# In[78]:


combi = train.append(test, ignore_index=True)


# In[82]:


combi.head()


# In[80]:


##function created to drop spedific characters:

def remove_pattern(input_txt, pattern):
    r = re.findall(pattern, input_txt)
    for i in r:
        input_txt = re.sub(i, '', input_txt)
        
    return input_txt


# In[81]:


# remove twitter handles (@user)
combi['tidy_tweet'] = np.vectorize(remove_pattern)(combi['tweet'], "@[\w]*")


# In[16]:


#A) Removing Twitter Handles (@user)
combi.head()


# In[18]:


#B) Removing Punctuations, Numbers, and Special Characters

combi['tidy_tweet'] = combi['tidy_tweet'].str.replace("[^a-zA-Z#]", " ")
combi.head()


# In[84]:


for i in range(1,15):
    print(combi['tweet'][i])


# In[85]:


for i in range(1,15):
    print(combi['tidy_tweet'][i])


# In[86]:


##C) Removing Short Words - length less or equal than 3

##so maybe this is their stop word process:



combi['tidy_tweet'] = combi['tidy_tweet'].apply(lambda x: ' '.join([w for w in x.split() if len(w)>3]))


# In[88]:


for i in range(1,15):
    print(combi['tidy_tweet'][i])


# In[89]:


##trying more cleaning:

combi['tidy_tweet'] = combi['tidy_tweet'].str.replace('[^\w\s]','')
combi['tidy_tweet'].head()


# In[90]:


for i in range(1,15):
    print(combi['tidy_tweet'][i])


# In[22]:


combi.head(10)


# In[30]:


#this is to check the values of the twitters:

for i in range(30,50):
    print(combi['tidy_tweet'][i])


# In[31]:


for i in range(30,50):
    print(combi['tweet'][i])


# In[33]:


##Term Vectors - tokenizations:

#Now we will tokenize all the cleaned tweets in our dataset. 
#Tokens are individual terms or words, and tokenization is the process of splitting a string of text into tokens.

#This is like my document term matrix???


tokenized_tweet = combi['tidy_tweet'].apply(lambda x: x.split())
tokenized_tweet.head(20)

#how can I see the class of an objet in python??


# In[34]:


##we don't have the spot words process here...dr. Healy web page is the one that has that info


# In[35]:


#E) Stemming
##Stemming is a rule-based process of stripping the suffixes (“ing”, “ly”, “es”, “s” etc) from a word. 
#For example, For example – “play”, “player”, “played”, “plays” and “playing” are the different variations of the word – “play”.


#THIS ONE TAKES A WHILE:


from nltk.stem.porter import *
stemmer = PorterStemmer()

tokenized_tweet = tokenized_tweet.apply(lambda x: [stemmer.stem(i) for i in x]) # stemming



# In[37]:


tokenized_tweet.head(20)


# In[39]:



#Now let’s stitch these tokens back together:

for i in range(len(tokenized_tweet)):
    tokenized_tweet[i] = ' '.join(tokenized_tweet[i])

combi['tidy_tweet'] = tokenized_tweet


# In[40]:


combi.head()


# In[48]:



##NOW SUPPOSELY WE HAVE CLEAN DATA, SO WE CAN BE ABLE TO TO ANY KIND OF ANALYSIS FROM NOW ON:


#3. Story Generation and Visualization from Tweets
#In this section, we will explore the cleaned tweets text. Exploring and visualizing data, no matter whether its text or any other data, is an essential step in gaining insights. Do not limit yourself to only these methods told in this tutorial, feel free to explore the data as much as possible.


all_words = ' '.join([text for text in combi['tidy_tweet']])
from wordcloud import WordCloud
wordcloud = WordCloud(width=800, height=500, random_state=21, max_font_size=110).generate(all_words)

plt.figure(figsize=(10, 7))
plt.imshow(wordcloud, interpolation="bilinear")
plt.axis('off')
plt.show()


# In[51]:


normal_words =' '.join([text for text in combi['tidy_tweet'][combi['label'] == 0]])

wordcloud = WordCloud(width=800, height=500, random_state=21, max_font_size=110).generate(normal_words)
plt.figure(figsize=(10, 7))
plt.imshow(wordcloud, interpolation="bilinear")
plt.axis('off')
plt.show()


# In[52]:


negative_words = ' '.join([text for text in combi['tidy_tweet'][combi['label'] == 1]])
wordcloud = WordCloud(width=800, height=500,
random_state=21, max_font_size=110).generate(negative_words)
plt.figure(figsize=(10, 7))
plt.imshow(wordcloud, interpolation="bilinear")
plt.axis('off')
plt.show()


# In[53]:


##D) Understanding the impact of Hashtags on tweets sentiment

# function to collect hashtags
def hashtag_extract(x):
    hashtags = []
    # Loop over the words in the tweet
    for i in x:
        ht = re.findall(r"#(\w+)", i)
        hashtags.append(ht)

    return hashtags


# In[55]:


# extracting hashtags from non racist/sexist tweets


HT_all = hashtag_extract(combi['tidy_tweet'])

HT_regular = hashtag_extract(combi['tidy_tweet'][combi['label'] == 0])

# extracting hashtags from racist/sexist tweets
HT_negative = hashtag_extract(combi['tidy_tweet'][combi['label'] == 1])

# unnesting list
HT_all = sum(HT_all,[])
HT_regular = sum(HT_regular,[])
HT_negative = sum(HT_negative,[])


# In[56]:


HT_all


# In[57]:


##Plotting the hashtags - regular:

a = nltk.FreqDist(HT_regular)
d = pd.DataFrame({'Hashtag': list(a.keys()),
                  'Count': list(a.values())})
# selecting top 10 most frequent hashtags     
d = d.nlargest(columns="Count", n = 10) 
plt.figure(figsize=(16,5))
ax = sns.barplot(data=d, x= "Hashtag", y = "Count")
ax.set(ylabel = 'Count')
plt.show()



# In[58]:


##Plotting the hashtags:

a = nltk.FreqDist(HT_all)
d = pd.DataFrame({'Hashtag': list(a.keys()),
                  'Count': list(a.values())})
# selecting top 10 most frequent hashtags     
d = d.nlargest(columns="Count", n = 10) 
plt.figure(figsize=(16,5))
ax = sns.barplot(data=d, x= "Hashtag", y = "Count")
ax.set(ylabel = 'Count')
plt.show()


# In[59]:


##Racist Tweets:

b = nltk.FreqDist(HT_negative)
e = pd.DataFrame({'Hashtag': list(b.keys()), 'Count': list(b.values())})
# selecting top 10 most frequent hashtags
e = e.nlargest(columns="Count", n = 10)   
plt.figure(figsize=(16,5))
ax = sns.barplot(data=e, x= "Hashtag", y = "Count")
ax.set(ylabel = 'Count')
plt.show()


# In[60]:



##4. Extracting Features from Cleaned Tweets:


#Bag-of-Words Features:

from sklearn.feature_extraction.text import CountVectorizer
bow_vectorizer = CountVectorizer(max_df=0.90, min_df=2, max_features=1000, stop_words='english')
# bag-of-words feature matrix
bow = bow_vectorizer.fit_transform(combi['tidy_tweet'])


# In[69]:



##Similarity


#Bag-of-Words Features
#Bag-of-Words is a method to represent text into numerical features. Consider a corpus (a collection of texts) 
#called C of D documents {d1,d2…..dD} and N unique tokens extracted out of the corpus C. 
#The N tokens (words) will form a list, and the size of the bag-of-words matrix M will be given by D X N. 
#Each row in the matrix M contains the frequency of tokens in document D(i).

from sklearn.feature_extraction.text import CountVectorizer
bow_vectorizer = CountVectorizer(max_df=0.90, min_df=2, max_features=1000, stop_words='english')
# bag-of-words feature matrix
bow = bow_vectorizer.fit_transform(combi['tidy_tweet'])



# In[70]:



#TF-IDF Features
#This is another method which is based on the frequency method but it is different to the bag-of-words approach 
#in the sense that it takes into account, not just the occurrence of a word in a single document (or tweet) 
#but in the entire corpus.

#TF-IDF works by penalizing the common words by assigning them lower weights while giving importance to words which are 
#rare in the entire corpus but appear in good numbers in few documents.

from sklearn.feature_extraction.text import TfidfVectorizer
tfidf_vectorizer = TfidfVectorizer(max_df=0.90, min_df=2, max_features=1000, stop_words='english')
# TF-IDF feature matrix
tfidf = tfidf_vectorizer.fit_transform(combi['tidy_tweet'])


# In[71]:


bow


# In[72]:


tfidf

