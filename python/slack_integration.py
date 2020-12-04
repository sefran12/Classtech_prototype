# -*- coding: utf-8 -*-
"""
Created on Wed Sep 16 15:04:35 2020

@author: Kilder
"""

##### SLACK CONNECTION

import slack
import pandas as pd
import matplotlib.pyplot as plt

SLACK_OAUTH_TOKEN = 'xoxb-1338486863700-1392289814576-gKtBPP00htpQdwfQAX9ZBIsp'
SLACK_CHANNEL = 'general'
client = slack.WebClient(token=SLACK_OAUTH_TOKEN)

def get_conversation_lists(client): 
    return client.conversations_list()

def get_channel_ids(client):
    conv_list = get_conversation_lists(client)
    return pd.DataFrame(conv_list.data['channels'])

def get_users_and_ids(client, channel):
    users_list = client.users_list(channel=channel)
    return pd.DataFrame(users_list.data['members'])

def get_conversation_history(client, channel):
    conv_hist = client.conversations_history(channel=channel)
    return pd.DataFrame(conv_hist.data['messages'])

def get_chat_data(client, channel):
    user_ids = get_users_and_ids(client, channel)
    chat_history = get_conversation_history(client, channel)
    return pd.merge(chat_history, user_ids[['id', 'real_name']], left_on='user', right_on='id').sort_values('ts', ascending = True)

channel_ids = get_channel_ids(client)
channel_ID_get = str(channel_ids[channel_ids.name == SLACK_CHANNEL].id.values[0])

user_ids = get_users_and_ids(client, channel_ID_get)
chat_data  = get_chat_data(client, channel_ID_get)
chat_data_fx  = get_chat_data(client, str(channel_ids[channel_ids.name == 'fx'].id.values[0]))

chat_data.to_csv('processed_data/slack_interactions_agri2020_2.csv', encoding = 'utf-8-sig')