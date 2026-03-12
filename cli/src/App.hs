module App (app) where


import Service.Application qualified as Application


app :: Application.Application
app = Application.new
