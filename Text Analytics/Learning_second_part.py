
# coding: utf-8

# In[1]:


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


# In[2]:


import sys
get_ipython().system('{sys.executable} -m pip install wordcloud')


# In[3]:



#this is where you have to put your files
#C:\Users\Carlos7

#this is the tutorial:
#https://www.analyticsvidhya.com/blog/2018/02/the-different-methods-deal-text-data-predictive-python/


# In[5]:


train  = pd.read_csv('train_E6oV3lV.csv')
test = pd.read_csv('test_tweets_anuFYb8.csv')


# In[6]:


#counting the number of words:

train['word_count'] = train['tweet'].apply(lambda x: len(str(x).split(" ")))
train[['tweet','word_count']].head()


# In[7]:


#counting the number of characters:

train['char_count'] = train['tweet'].str.len() ## this also includes spaces
train[['tweet','char_count']].head()


# In[ ]:


import nltk
nltk.download('stopwords')


# In[11]:


#1.4 Number of stopwords:  
#we are calling the stopwords in here:


from nltk.corpus import stopwords
stop = stopwords.words('english')

train['stopwords'] = train['tweet'].apply(lambda x: len([x for x in x.split() if x in stop]))
train[['tweet','stopwords']].head()


# In[12]:


##Number of hashtags:

train['hastags'] = train['tweet'].apply(lambda x: len([x for x in x.split() if x.startswith('#')]))
train[['tweet','hastags']].head()



# In[14]:


##Number of numerics:

train['numerics'] = train['tweet'].apply(lambda x: len([x for x in x.split() if x.isdigit()]))
train[['tweet','numerics']].head(40)


# In[17]:


#Remember that python starts with zero:

for i in range(37,40):
    print(train['tweet'][i])


# In[18]:


##Number of Uppercase words:

train['upper'] = train['tweet'].apply(lambda x: len([x for x in x.split() if x.isupper()]))
train[['tweet','upper']].head()


# In[19]:






##HERE THE REAL CLEANING PROCESS BEGINS:



# In[20]:


#2.1 Lower case
train['tweet'] = train['tweet'].apply(lambda x: " ".join(x.lower() for x in x.split()))
train['tweet'].head()


# In[23]:


#2.2 Removing Punctuation

train['tweet'] = train['tweet'].str.replace('[^\w\s]','')
train['tweet'].head()


# In[25]:


for i in range(1,15):
    print(train['tweet'][i])
#2.4 Common word removal


# In[26]:


###2.4 Common word removal

freq = pd.Series(' '.join(train['tweet']).split()).value_counts()[:10]


# In[27]:


freq


# In[28]:


freq = list(freq.index)
train['tweet'] = train['tweet'].apply(lambda x: " ".join(x for x in x.split() if x not in freq))


# In[29]:


for i in range(1,15):
    print(train['tweet'][i])


# In[30]:


##less common words:

freq = pd.Series(' '.join(train['tweet']).split()).value_counts()[-10:]


# In[31]:


freq


# In[32]:


freq = list(freq.index)
train['tweet'] = train['tweet'].apply(lambda x: " ".join(x for x in x.split() if x not in freq))
train['tweet'].head()


# In[33]:


for i in range(1,15):
    print(train['tweet'][i])


# In[ ]:


##Spelling correction
####do we need this???


# In[36]:


import sys
get_ipython().system('{sys.executable} -m pip install textblob')


# In[38]:


##2.7 Tokenization
from textblob import TextBlob


# In[40]:


import nltk
nltk.download('punkt')


# In[42]:


##2.7 Tokenization


TextBlob(train['tweet'][1]).words


# In[43]:


##2.8 Stemming

from nltk.stem import PorterStemmer
st = PorterStemmer()
train['tweet'][:5].apply(lambda x: " ".join([st.stem(word) for word in x.split()]))


# In[44]:


for i in range(1,15):
    print(train['tweet'][i])

