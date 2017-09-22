from sklearn.cluster import KMeans
from optparse import OptionParse
import csv

parser = OptionParser()
parser.add_option('-c', '--clusters', dest='opt_n_clusters', type='int',
    help='Number of clusters (2 to 8).', metavar='NUMBER')
(options, args) = parser.parse_args()

def get_data():
    points = []

    with open('sample_car_dealer_visits.csv','r') as csvfile:
        fieldnames = ['dt','ts','lat','lng','thoroughfare','place_name']
        reader = csv.DictReader(csvfile)
        for row in reader:
            points.append([row[fieldnames[2]], row[fieldnames[3]]])

    return points

def set_points(points):
    with open('cluster_centers.csv','w') as csvfile:
        fieldnames = ['lat','lng']
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()
        for point in points:
            writer.writerow({fieldnames[0]: point[0], 
                fieldnames[1]: point[1]})

def main(n_clusters):
    data = get_data()
    kmeans = KMeans(n_clusters=n_clusters).fit(data)
    set_points(kmeans.cluster_centers_)

if __name__ == '__main__':
    main(options.opt_n_clusters)