import os 
import random
import numpy as np
import matplotlib.pyplot as plt
os.getcwd() 
os.chdir("/Users/benjamin/Python/ASA")

# import pckl file
import pickle
maze = open("maze20x15.pckl", "rb")
maze = pickle.load(maze)
maze.shape
print(maze)

# define a function that finds the boundary points that are equal to True, and returns the coordinates of those points
def find_boundary_points(maze):
    boundary_points = []
    for i in range(maze.shape[0]):
        for j in range(maze.shape[1]):
            if maze[i,j] == True and (i == 0 or j == 0 or i == maze.shape[0]-1 or j == maze.shape[1]-1):
                boundary_points.append([i,j])
    return boundary_points

boundary_points = find_boundary_points(maze)

start_cell = boundary_points[0]
final_cell = tuple(boundary_points[1])

current_cell = tuple(start_cell)
realized_path = [] #list of cells that have been visited
realized_path.append(current_cell)  

def maze_solver(maze):
    # Part I: Find boundary points 
    boundary_points = find_boundary_points(maze)
    start_cell = boundary_points[0]
    final_cell = tuple(boundary_points[1])

    current_cell = tuple(start_cell)
    realized_path = [] #list of cells that have been visited
    realized_path.append(current_cell)  
    # a while loop where the statement is current_cell not equal to final_cell
    while current_cell != final_cell:
    # Part II: create a list of all neighboring cells that are not walls
        neighbors = []
        if maze[current_cell[0]-1, current_cell[1]] == True:
            neighbors.append((abs(current_cell[0]-1), current_cell[1]))
        if maze[current_cell[0]+1, current_cell[1]] == True:
            neighbors.append((current_cell[0]+1, current_cell[1]))
        if maze[current_cell[0], current_cell[1]-1] == True:
            neighbors.append((current_cell[0], current_cell[1]-1))
        if maze[current_cell[0], current_cell[1]+1] == True:
            neighbors.append((current_cell[0], current_cell[1]+1))
        #Part III: Decision tree
        # if one of the cells in neighbor is in realized path, remove it from the list of neighbors
        if len(neighbors) >= 2 and (not set(neighbors).isdisjoint(realized_path)):
            intersection = set(neighbors).intersection(set(realized_path))
            if set(neighbors).issubset(set(realized_path)):
                if max([realized_path.count(i) for i in neighbors]) >= 4:
                    lowest_index = min([realized_path.index(i) for i in intersection])
                    neighbors.remove(realized_path[lowest_index])
                    if max([realized_path.count(i) for i in neighbors]) >= 10:
                        neighbors = [realized_path[random.randint(0,len(realized_path)-1)]]
                else:
                    # choose the cell with the lowest index in realized path
                    lowest_index = min([realized_path.index(i) for i in intersection]) # to make sure to escape dead ends
                    neighbors = []
                    neighbors.append(realized_path[lowest_index])
            else:
                for i in intersection:
                    neighbors.remove(i)
        # randomly choose one of the neighbors
        current_cell = neighbors[random.randint(0,len(neighbors)-1)]
        realized_path.append(current_cell)
    results = [realized_path, start_cell, final_cell]
    return(results)

maze_solver(maze)

results = maze_solver(maze)
realized_path = results[0]
start_cell = results[1]
final_cell = results[2]

# plot each step of the path
def plot_path(maze, realized_path, start_cell, final_cell):
    plt.imshow(maze, cmap = 'binary')
    plt.plot(start_cell[1], start_cell[0], 'ro')
    plt.plot(final_cell[1], final_cell[0], 'ro')
    for i in realized_path:
        plt.plot(i[1], i[0], 'bo')
        plt.pause(0.01)
    plt.show()



# plot the path
plt.imshow(maze, cmap='gray')
plt.plot([i[1] for i in realized_path[:50]], [i[0] for i in realized_path[:50]], color='red', label = 'Path')
plt.plot(start_cell[1], start_cell[0], 'o', color='green', label = 'Start')
plt.plot(final_cell[1], final_cell[0], 'o', color='blue', label = 'End')
plt.legend(bbox_to_anchor =(1.4, 0.7))
plt.show()

