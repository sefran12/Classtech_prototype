# -*- coding: utf-8 -*-
"""
Created on Wed Oct 28 20:32:28 2020

@author: Kilder
"""

"""
apiKey: "AIzaSyARFJJr7NmdqCOXq5zR7edrADYeN9h-L4Q",
      authDomain: "engagementmonitor.firebaseapp.com",
      databaseURL: "https://engagementmonitor.firebaseio.com",
      projectId: "engagementmonitor",
      storageBucket: "engagementmonitor.appspot.com",
      messagingSenderId: "745554810373",
      appId: "1:745554810373:web:6ddd83c466f0e064196e6c",
      measurementId: "G-CK1ZDRS
 """
 
import pyrebase
import pandas as pd

email = "sefran12@gmail.com"
password = "123456789"
config = {
  "apiKey": "AIzaSyARFJJr7NmdqCOXq5zR7edrADYeN9h-L4Q",
  "authDomain": "engagementmonitor.firebaseapp.com",
  "databaseURL": "https://engagementmonitor.firebaseio.com",
  "projectId": "engagementmonitor",
  "storageBucket": "engagementmonitor.appspot.com"
}

firebase = pyrebase.initialize_app(config)
db = firebase.database()

# Get a reference to the auth service
auth = firebase.auth()

# Log the user in
user = auth.sign_in_with_email_and_password(email, password)

# Get a reference to the database service
db = firebase.database()

db_full = db.get()
db_users = db.child("Alumno").get()

keys = []
vals = []
for user in db_full.each():
    keys.append(user.key())
    vals.append(user.val())
    
notas = vals[0]
notas_df = pd.DataFrame(notas)
notas_df = notas_df.transpose()
notas_df = notas_df.drop('grades', 1).assign(**notas_df.grades.dropna().apply(pd.Series))
