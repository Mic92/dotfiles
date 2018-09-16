import requests
from datetime import datetime
from influxdb import InfluxDBClient

url = "http://matemat.hq.c3d2.de/{}"

def main():
    resp = requests.get(url.format("backup/inventory.json"))
    fields = {}
    tags = {}
    json_body = []

    for article in resp.json():
        json_body.append(dict(
            measurement="inventory",
            tags=dict(name=article["name"]),
            fields=dict(filter(lambda t: t[0] not in ["artNr", "name"], article.items()))
        ))

    client = InfluxDBClient('influxdb.thalheim.io',
             port=8086,
             ssl=True,
             username="matemat",
             password="eig0NaGoahCia5oo",
             database="matemat")
    client.write_points(json_body)
    resp2 = requests.get(url.format("statistics.json"))
    json_body = resp2.json()

    if "total_balance" in json_body:
        json_body["total_balance"] = float(json_body["total_balance"])
    if "total_loss_retail_price" in json_body:
        json_body["total_loss_retail_price"] = float(json_body["total_loss_retail_price"])
    if "positive_balance" in json_body:
        json_body["positive_balance"] = float(json_body["positive_balance"])
    if "negative_balance" in json_body:
        json_body["negative_balance"] = int(json_body["negative_balance"])

    if "inactive_users_negative_balance" in json_body:
        json_body["inactive_users_negative_balance"] = float(json_body["inactive_users_negative_balance"])

    client.write_points([dict(measurement="statistics", fields=json_body, tags={})])

if __name__ == "__main__":
    main()
