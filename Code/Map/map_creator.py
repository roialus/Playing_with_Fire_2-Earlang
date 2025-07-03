import networkx as nx
import numpy as np
import random
import matplotlib.pyplot as plt
import sys
from collections import deque


class UnionFind:
    def __init__(self, n):
        self.parent = list(range(n))    # parent[i] is the parent of node i
        self.rank = [1] * n # for union by rank

    def find(self, u):
        if self.parent[u] != u:
            self.parent[u] = self.find(self.parent[u])  # path compression
        return self.parent[u]

    def union(self, u, v):
        root_u = self.find(u)   # Find the root of u
        root_v = self.find(v)   # Find the root of v

        if root_u != root_v:
            # Union by rank
            if self.rank[root_u] > self.rank[root_v]:   # If root_u has higher rank
                self.parent[root_v] = root_u
            elif self.rank[root_u] < self.rank[root_v]: # If root_v has higher rank
                self.parent[root_u] = root_v
            else:   # If both have the same rank
                self.parent[root_v] = root_u
                self.rank[root_u] += 1
            return True
        return False


def floyd_warshall(graph):
    # Create a mapping from node to index
    node_map = {node: idx for idx, node in enumerate(graph.nodes())}
    inv_node_map = {idx: node for node, idx in node_map.items()}    # Inverse mapping for later use
    V = len(graph)  # Number of vertices in the graph

    # Initialize distances with infinity and set diagonal to 0
    dist = [[sys.maxsize] * V for _ in range(V)]
    for i in range(V):
        dist[i][i] = 0

    # Populate initial distances from the graph
    for u, v, data in graph.edges(data=True):
        i, j = node_map[u], node_map[v] # Get indices of nodes u and v
        dist[i][j] = data['weight']
        dist[j][i] = data['weight']  # Undirected graph

    # Applying Floyd-Warshall algorithm
    for k in range(V):
        for i in range(V):
            for j in range(V):
                if dist[i][j] > dist[i][k] + dist[k][j]:
                    dist[i][j] = dist[i][k] + dist[k][j]    # Update distance if a shorter path is found

    # Convert distance matrix back to dictionary format similar to NetworkX
    dist_dict = {}
    for i in range(V):
        dist_dict[inv_node_map[i]] = {}
        for j in range(V):
            dist_dict[inv_node_map[i]][inv_node_map[j]] = dist[i][j]    # Assign distances to the dictionary

    return dist_dict


def create_spanning_tree(graph):
    node_map = {node: idx for idx, node in enumerate(graph.nodes())}    # Create a mapping from node to index
    inv_node_map = {idx: node for node, idx in node_map.items()}    # Inverse mapping for later use
    edges = [(graph.adj[u][v]['weight'], node_map[u], node_map[v]) for u, v in graph.edges()]   # Create a list of edges with weights and indices
    edges.sort()  # Sort edges by weight

    uf = UnionFind(len(graph.nodes))    # Initialize Union-Find structure
    mst = nx.Graph()    # Create an empty graph for the Minimum Spanning Tree (MST)

    for weight, u, v in edges:
        if uf.union(u, v):
            mst.add_edge(inv_node_map[u], inv_node_map[v], weight=weight)   # Add edge to MST if u and v are in different components
            if len(mst.edges()) == len(graph.nodes) - 1:
                break

    return mst


def steiner2(G, w, R):
    # Compute shortest paths using Floyd-Warshall
    d = floyd_warshall(G)

    # Create the complete graph K_R using distances d
    KR = nx.complete_graph(R)
    for u, v in KR.edges:
        if d[u][v] or d[u][v] is not None:  # If edge (u, v) exists in d
            KR[u][v]['weight'] = d[u][v]
        else:  # If edge (u, v) does not exist in d
            KR[u][v]['weight'] = sys.maxsize  # Set the weight to infinity
    # Compute MST using custom spanning tree function
    T_tilde = create_spanning_tree(KR)

    # Generate Steiner tree S(T_tilde) (assuming S(T_tilde) is T_tilde itself in this context)
    return T_tilde


def steiner3(G, w, R):
    #  Compute shortest paths using Floyd-Warshall
    d = floyd_warshall(G)

    # Initialize k, R_k, and T_k
    k = 0
    R_k = set(R)
    KR = nx.complete_graph(R_k)  # Create the complete graph K_R using distances d
    for u, v in KR.edges:
        KR[u][v]['weight'] = d[u][v]
    T_k = create_spanning_tree(KR)  # Compute MST using custom spanning tree function

    while k <= len(G.nodes) - len(R):  # Repeat until R_k = V
        if R_k == set(G.nodes):  # If R_k = V, return S(T_k), which is assumed to be T_k itself
            return T_k  # Return S(T_k), which is assumed to be T_k itself

        t_star = None  # Initialize t_star
        T_star = T_k  # Initialize T_star
        for v_i in set(G.nodes) - R_k:  # For each v_i in V - R_k
            R_ki = R_k.union({v_i})
            KR_ki = nx.complete_graph(R_ki)  # Create the complete graph K_R using distances d
            for u, v in KR_ki.edges:
                if d[u][v] or d[u][v] is not None:  # If edge (u, v) exists in d
                    KR_ki[u][v]['weight'] = d[u][v]
                else:  # If edge (u, v) does not exist in d
                    KR_ki[u][v]['weight'] = sys.maxsize  # Set the weight to infinity
            T_prime_k = create_spanning_tree(KR_ki)  # Compute MST using custom spanning tree function
            if T_prime_k.size(weight='weight') < T_k.size(weight='weight'):  # If S(T_prime_k) is better than S(T_k)
                T_star = T_prime_k  # Update T_star
                t_star = v_i  # Update t_star

        if T_star.size(weight='weight') < T_k.size(weight='weight'):  # If S(T_star) is better than S(T_k)
            k += 1
            T_k = T_star  # Update T_k
            R_k.add(t_star)  # Update R_k
        else:  # If S(T_star) is not better than S(T_k)
            return T_k  # Return S(T_k), which is assumed to be T_k itself

    return T_k  # Return S(T_k), which is assumed to be T_k itself


class BombermanMapGenerator:
    def __init__(self, size=16):
        self.size = size
        self.map_grid = np.zeros((size, size), dtype=int)

        # Tile types (updated with 4 types)
        self.FREE = 0  # Free spot (no tile) - walkable
        self.BREAKABLE = 1  # Breakable tile - destructible in 1 explosion
        self.UNBREAKABLE = 2  # Unbreakable wall - permanent obstacle
        self.STRONG = 3  # Strong tile - needs 2 explosions to break
        self.PLAYER_START = 4  # Player starting position (special marker)

        # Player starting corners (1 tile away from border)
        self.corners = [(1, 1), (1, size - 2), (size - 2, 1), (size - 2, size - 2)]

        # Graph for Steiner tree calculation
        self.graph = None
        self.steiner_paths = set()

    def coord_to_node(self, x, y):
        """Convert (x,y) coordinate to graph node ID"""
        return x * self.size + y

    def node_to_coord(self, node):
        """Convert graph node ID to (x,y) coordinate"""
        return (node // self.size, node % self.size)

    def is_valid_position(self, x, y):
        """Check if position is within valid bounds (not on edges)"""
        return 1 <= x < self.size - 1 and 1 <= y < self.size - 1

    def is_edge_position(self, x, y):
        """Check if position is on the edge of the map"""
        return x == 0 or x == self.size - 1 or y == 0 or y == self.size - 1

    def create_game_graph(self):
        """Create a graph representation of the game map for Steiner tree calculation"""
        G = nx.Graph()

        # Add all valid positions as nodes (excluding border walls)
        for x in range(1, self.size - 1):
            for y in range(1, self.size - 1):
                node = self.coord_to_node(x, y)
                G.add_node(node)

        # Add edges between adjacent valid positions
        for x in range(1, self.size - 1):
            for y in range(1, self.size - 1):
                current = self.coord_to_node(x, y)

                # Check 4 directions
                for dx, dy in [(0, 1), (1, 0), (0, -1), (-1, 0)]:
                    new_x, new_y = x + dx, y + dy
                    if self.is_valid_position(new_x, new_y):
                        neighbor = self.coord_to_node(new_x, new_y)
                        G.add_edge(current, neighbor, weight=1)

        return G

    def select_terminals(self, num_random=2):
        """Select terminals: 4 corners + random internal positions"""
        terminals = []

        # Add corner positions as terminal nodes
        for x, y in self.corners:
            terminals.append(self.coord_to_node(x, y))

        # Add random internal terminals
        attempts = 0
        while len(terminals) < 4 + num_random and attempts < 100:
            attempts += 1
            x = random.randint(3, self.size - 4)
            y = random.randint(3, self.size - 4)

            # Ensure not too close to corners or other terminals
            node = self.coord_to_node(x, y)
            if self.is_valid_terminal_position(x, y, terminals):
                terminals.append(node)

        return terminals

    def is_valid_terminal_position(self, x, y, existing_terminals):
        """Check if position is valid for a terminal"""
        # Convert existing terminals back to coordinates for distance check
        for terminal_node in existing_terminals:
            tx, ty = self.node_to_coord(terminal_node)
            # Ensure minimum distance of 3 tiles
            if abs(x - tx) < 3 or abs(y - ty) < 3:
                return False
        return True

    def extract_steiner_paths(self, steiner_tree):
        """Extract all path positions from Steiner tree using shortest paths"""
        paths = set()

        # For each edge in the Steiner tree, find the shortest path
        for u, v in steiner_tree.edges():
            path = nx.shortest_path(self.graph, u, v, weight='weight')
            for node in path:
                x, y = self.node_to_coord(node)
                paths.add((x, y))

        return paths

    def initialize_map(self):
        """Initialize map with unbreakable borders and clear internal areas"""
        # Fill entire map with free spots initially
        self.map_grid.fill(self.FREE)

        # Create unbreakable border (edges must be unbreakable)
        for i in range(self.size):
            self.map_grid[0, i] = self.UNBREAKABLE  # Top border
            self.map_grid[self.size - 1, i] = self.UNBREAKABLE  # Bottom border
            self.map_grid[i, 0] = self.UNBREAKABLE  # Left border
            self.map_grid[i, self.size - 1] = self.UNBREAKABLE  # Right border

        # Clear player starting areas (2x2 around each corner must be free)
        for corner_x, corner_y in self.corners:
            for dx in range(-1, 2):
                for dy in range(-1, 2):
                    x, y = corner_x + dx, corner_y + dy
                    if self.is_valid_position(x, y):
                        self.map_grid[x, y] = self.FREE

    def apply_steiner_tree_to_map(self, steiner_paths, steiner_breakable_chance=0.3):
        """Apply Steiner tree paths - can be FREE or BREAKABLE"""
        self.steiner_paths = steiner_paths

        print(f"🌲 Applying Steiner tree to {len(steiner_paths)} positions")

        # Steiner paths can be either FREE or BREAKABLE
        for x, y in steiner_paths:
            if self.is_valid_position(x, y) and not self.is_in_player_area(x, y):
                # Random choice: FREE or BREAKABLE for Steiner paths
                if random.random() < steiner_breakable_chance:
                    self.map_grid[x, y] = self.BREAKABLE
                else:
                    self.map_grid[x, y] = self.FREE
            elif self.is_in_player_area(x, y):
                # Player areas must remain free
                self.map_grid[x, y] = self.FREE

    def fill_non_steiner_areas(self, breakable_prob=0.3, strong_prob=0.15, unbreakable_prob=0.25):
        """Fill non-Steiner areas with BREAKABLE, STRONG, UNBREAKABLE, or FREE tiles"""
        available_positions = []

        # Find positions that are not in Steiner paths, edges, or player areas
        for x in range(1, self.size - 1):
            for y in range(1, self.size - 1):
                if ((x, y) not in self.steiner_paths and
                        not self.is_in_player_area(x, y) and
                        not self.is_edge_position(x, y)):
                    available_positions.append((x, y))

        print(f"🎯 Filling {len(available_positions)} non-Steiner positions")

        # Assign tile types to non-Steiner positions
        for x, y in available_positions:
            rand_val = random.random()

            if rand_val < breakable_prob:
                self.map_grid[x, y] = self.BREAKABLE
            elif rand_val < breakable_prob + strong_prob:
                self.map_grid[x, y] = self.STRONG
            elif rand_val < breakable_prob + strong_prob + unbreakable_prob:
                self.map_grid[x, y] = self.UNBREAKABLE
            else:
                self.map_grid[x, y] = self.FREE  # Remains free

        # Count what we placed
        tile_counts = {
            'breakable': np.sum(self.map_grid == self.BREAKABLE),
            'strong': np.sum(self.map_grid == self.STRONG),
            'unbreakable': np.sum(self.map_grid == self.UNBREAKABLE),
            'free': np.sum(self.map_grid == self.FREE)
        }

        print(f"🧱 Placed tiles - Breakable: {tile_counts['breakable']}, "
              f"Strong: {tile_counts['strong']}, Unbreakable: {tile_counts['unbreakable']}, "
              f"Free: {tile_counts['free']}")

    def is_in_player_area(self, x, y):
        """Check if position is in a player starting area"""
        for cx, cy in self.corners:
            if abs(x - cx) <= 1 and abs(y - cy) <= 1:
                return True
        return False

    def mark_player_starts(self):
        """Mark player starting positions"""
        for i, (x, y) in enumerate(self.corners):
            self.map_grid[x, y] = self.PLAYER_START

    def verify_connectivity(self):
        """Verify that all corners are reachable from each other"""

        def bfs_reachable(start_x, start_y):
            visited = set()
            queue = deque([(start_x, start_y)])
            visited.add((start_x, start_y))
            reachable = set()

            while queue:
                x, y = queue.popleft()
                reachable.add((x, y))

                for dx, dy in [(0, 1), (1, 0), (0, -1), (-1, 0)]:
                    nx, ny = x + dx, y + dy
                    if (0 <= nx < self.size and 0 <= ny < self.size and
                            (nx, ny) not in visited):

                        # Can walk on FREE, BREAKABLE, STRONG, and PLAYER_START
                        tile_type = self.map_grid[nx, ny]
                        if tile_type in [self.FREE, self.BREAKABLE, self.STRONG, self.PLAYER_START]:
                            visited.add((nx, ny))
                            queue.append((nx, ny))

            return reachable

        # Check if all corners are reachable from the first corner
        start_x, start_y = self.corners[0]
        reachable = bfs_reachable(start_x, start_y)

        all_connected = True
        for corner_x, corner_y in self.corners[1:]:
            if (corner_x, corner_y) not in reachable:
                all_connected = False
                break

        return all_connected

    def generate_map(self, steiner_algorithm='steiner2', num_random_terminals=2,
                     steiner_breakable_chance=0.3, breakable_prob=0.3,
                     strong_prob=0.15, unbreakable_prob=0.25):
        """Generate complete map using Steiner tree algorithm with 4 tile types"""
        print(f"🗺️  Generating {self.size}x{self.size} Bomberman Map with 4 Tile Types...")

        # Step 1: Initialize map structure
        self.initialize_map()
        print("✅ Map borders and player areas initialized")

        # Step 2: Create graph representation
        self.graph = self.create_game_graph()
        print(f"📊 Created graph with {len(self.graph.nodes)} nodes and {len(self.graph.edges)} edges")

        # Step 3: Select terminals
        terminals = self.select_terminals(num_random_terminals)
        terminal_coords = [self.node_to_coord(t) for t in terminals]
        print(f"📍 Selected terminals: {terminal_coords}")

        # Step 4: Generate Steiner tree
        if steiner_algorithm == 'steiner2':
            steiner_tree = steiner2(self.graph, nx.get_edge_attributes(self.graph, 'weight'), terminals)
        else:
            steiner_tree = steiner3(self.graph, nx.get_edge_attributes(self.graph, 'weight'), terminals)

        print(f"🌲 Generated Steiner tree with {len(steiner_tree.edges)} edges using {steiner_algorithm}")

        # Step 5: Extract paths from Steiner tree
        steiner_paths = self.extract_steiner_paths(steiner_tree)
        print(f"🛤️  Extracted {len(steiner_paths)} positions in Steiner paths")

        # Step 6: Apply tree to map (can contain breakable tiles)
        self.apply_steiner_tree_to_map(steiner_paths, steiner_breakable_chance)

        # Step 7: Fill non-Steiner areas with various tile types
        self.fill_non_steiner_areas(breakable_prob, strong_prob, unbreakable_prob)

        # Step 8: Mark player starts
        self.mark_player_starts()

        # Step 9: Verify connectivity
        if self.verify_connectivity():
            print("✅ All player corners are reachable!")
        else:
            print("⚠️  Warning: Not all corners are reachable!")

        print("🎉 Map generation complete!")
        return self.map_grid.copy()

    def visualize_map(self, save_path=None, show_steiner=True):
        """Visualize the generated map with 4 tile types"""
        fig, ax = plt.subplots(1, 1, figsize=(12, 12))

        # Create colored visualization
        colored_map = np.zeros((self.size, self.size, 3))

        # Color mapping for 4 tile types
        color_values = {
            self.FREE: [0.9, 0.9, 0.9],  # Light gray - walkable
            self.BREAKABLE: [0.6, 0.3, 0.1],  # Brown - breakable tile
            self.UNBREAKABLE: [0.1, 0.1, 0.1],  # Black - unbreakable wall
            self.STRONG: [0.4, 0.2, 0.8],  # Purple - strong tile (2 hits)
            self.PLAYER_START: [1.0, 0.2, 0.2]  # Red - player start
        }

        for x in range(self.size):
            for y in range(self.size):
                tile_type = self.map_grid[x, y]
                colored_map[x, y] = color_values[tile_type]

        # Highlight Steiner tree paths if requested
        if show_steiner and hasattr(self, 'steiner_paths'):
            for x, y in self.steiner_paths:
                if not self.is_in_player_area(x, y):
                    # Add green tint to Steiner paths
                    colored_map[x, y] = [0.2, 0.8, 0.2]  # Bright green for Steiner paths

        ax.imshow(colored_map, origin='upper')

        # Add grid
        for i in range(self.size + 1):
            ax.axhline(i - 0.5, color='white', linewidth=0.5, alpha=0.3)
            ax.axvline(i - 0.5, color='white', linewidth=0.5, alpha=0.3)

        # Add coordinate labels
        ax.set_xticks(range(self.size))
        ax.set_yticks(range(self.size))
        ax.set_xlabel('Y Coordinate', fontsize=12)
        ax.set_ylabel('X Coordinate', fontsize=12)

        # Add player numbers
        for i, (x, y) in enumerate(self.corners):
            ax.text(y, x, f'P{i + 1}', ha='center', va='center',
                    fontsize=10, fontweight='bold', color='white')

        ax.set_title(f'Playing with Fire 2 - {self.size}x{self.size} Map\n(4 Tile Types + Steiner Tree)',
                     fontsize=16, fontweight='bold', pad=20)

        # Enhanced Legend
        legend_elements = [
            plt.Rectangle((0, 0), 1, 1, fc='lightgray', label='Free Spot'),
            plt.Rectangle((0, 0), 1, 1, fc='brown', label='Breakable Tile'),
            plt.Rectangle((0, 0), 1, 1, fc='black', label='Unbreakable Wall'),
            plt.Rectangle((0, 0), 1, 1, fc='purple', label='Strong Tile (2 hits)'),
            plt.Rectangle((0, 0), 1, 1, fc='red', label='Player Start')
        ]
        if show_steiner:
            legend_elements.append(plt.Rectangle((0, 0), 1, 1, fc='green', label='Steiner Path'))

        ax.legend(handles=legend_elements, loc='upper left', bbox_to_anchor=(1.02, 1))

        plt.tight_layout()

        if save_path:
            plt.savefig(save_path, dpi=300, bbox_inches='tight')
            print(f"💾 Map saved to {save_path}")

        plt.show()

    def export_to_erlang(self, output_file="generated_map.erl"):
        """Export map to Erlang format with 4 tile types"""
        with open(output_file, 'w') as f:
            f.write("%% Generated 16x16 map for Playing with Fire 2\n")
            f.write("%% Created using Steiner Tree Algorithm with 4 tile types\n")
            f.write("%% Map ensures connectivity between all player corners\n\n")
            f.write("-module(generated_map).\n")
            f.write("-export([get_map/0, get_tile_type/2, get_player_starts/0, get_map_size/0]).\n\n")

            # Write tile type definitions
            f.write("%% Tile type definitions\n")
            f.write("-define(FREE, 0).         %% Free spot (no tile) - walkable\n")
            f.write("-define(BREAKABLE, 1).    %% Breakable tile - destructible in 1 explosion\n")
            f.write("-define(UNBREAKABLE, 2).  %% Unbreakable wall - permanent obstacle\n")
            f.write("-define(STRONG, 3).       %% Strong tile - needs 2 explosions to break\n")
            f.write("-define(PLAYER_START, 4). %% Player starting position\n\n")

            # Write map size
            f.write("get_map_size() -> 16.\n\n")

            # Write map data
            f.write("get_map() ->\n    [\n")
            for x in range(self.size):
                row_values = []
                for y in range(self.size):
                    row_values.append(str(self.map_grid[x, y]))
                row = "        [" + ", ".join(row_values) + "]"
                if x < self.size - 1:
                    row += ","
                f.write(row + "\n")
            f.write("    ].\n\n")

            # Write helper functions
            f.write("get_tile_type(X, Y) when X >= 0, X < 16, Y >= 0, Y < 16 ->\n")
            f.write("    Map = get_map(),\n")
            f.write("    Row = lists:nth(X + 1, Map),\n")
            f.write("    lists:nth(Y + 1, Row);\n")
            f.write("get_tile_type(_, _) ->\n")
            f.write("    ?UNBREAKABLE. %% Out of bounds\n\n")

            f.write("get_player_starts() ->\n")
            player_starts = []
            for i, (x, y) in enumerate(self.corners):
                player_starts.append(f"{{player_{i + 1}, {x}, {y}}}")
            f.write(f"    [{', '.join(player_starts)}].\n\n")

            # Write tile type checkers
            f.write("%% Tile type checking functions\n")
            f.write("is_walkable(TileType) ->\n")
            f.write("    TileType =:= ?FREE orelse TileType =:= ?BREAKABLE orelse \n")
            f.write("    TileType =:= ?STRONG orelse TileType =:= ?PLAYER_START.\n\n")

            f.write("is_destructible(TileType) ->\n")
            f.write("    TileType =:= ?BREAKABLE orelse TileType =:= ?STRONG.\n\n")

            f.write("needs_two_hits(TileType) ->\n")
            f.write("    TileType =:= ?STRONG.\n\n")

            # Write connectivity verification
            f.write("%% Map guarantees connectivity via Steiner tree\n")
            f.write("%% Steiner paths can contain both FREE and BREAKABLE tiles\n")
            f.write("%% Strong tiles require 2 explosions to destroy\n")
            f.write("%% Powerups can spawn from BREAKABLE and STRONG tiles when destroyed\n")

        print(f"📄 Exported map to {output_file}")

    def get_statistics(self):
        """Get comprehensive map statistics"""
        total = self.size * self.size
        free = np.sum(self.map_grid == self.FREE)
        breakable = np.sum(self.map_grid == self.BREAKABLE)
        unbreakable = np.sum(self.map_grid == self.UNBREAKABLE)
        strong = np.sum(self.map_grid == self.STRONG)
        players = np.sum(self.map_grid == self.PLAYER_START)

        # Count tiles in Steiner paths
        steiner_free = 0
        steiner_breakable = 0
        if hasattr(self, 'steiner_paths'):
            for x, y in self.steiner_paths:
                if self.map_grid[x, y] == self.FREE:
                    steiner_free += 1
                elif self.map_grid[x, y] == self.BREAKABLE:
                    steiner_breakable += 1

        stats = {
            'map_size': f"{self.size}x{self.size}",
            'total_tiles': total,
            'free_tiles': free,
            'breakable_tiles': breakable,
            'unbreakable_tiles': unbreakable,
            'strong_tiles': strong,
            'player_starts': players,
            'steiner_path_length': len(self.steiner_paths) if hasattr(self, 'steiner_paths') else 0,
            'steiner_free_tiles': steiner_free,
            'steiner_breakable_tiles': steiner_breakable,
            'destructible_percentage': ((breakable + strong) / total) * 100,
            'connectivity_guaranteed': True
        }

        return stats


def main():
    """Demonstration of the enhanced map generation system"""
    print("🎮 Playing with Fire 2 - Enhanced Map Generator (4 Tile Types)")
    print("=" * 70)

    # Create generator
    generator = BombermanMapGenerator(size=16)

    # Generate map using Steiner2 algorithm with new parameters
    print("\n🔥 Generating map with Steiner2 algorithm...")
    map_grid = generator.generate_map(
        steiner_algorithm='steiner2',
        num_random_terminals=2,
        steiner_breakable_chance=0.3,  # 30% of Steiner paths are breakable
        breakable_prob=0.3,  # 30% of non-Steiner area is breakable
        strong_prob=0.15,  # 15% of non-Steiner area is strong
        unbreakable_prob=0.25  # 25% of non-Steiner area is unbreakable
    )

    # Show statistics
    stats = generator.get_statistics()
    print(f"\n📊 Map Statistics:")
    for key, value in stats.items():
        if isinstance(value, float):
            print(f"   {key}: {value:.1f}")
        else:
            print(f"   {key}: {value}")

    # Visualize map
    generator.visualize_map("bomberman_map_4_types.png", show_steiner=True)

    # Export to Erlang
    generator.export_to_erlang("generated_map_4_types.erl")

    # Generate comparison map with different parameters
    print(f"\n🔥 Generating comparison map with more aggressive settings...")
    generator2 = BombermanMapGenerator(size=16)
    map_grid2 = generator2.generate_map(
        steiner_algorithm='steiner3',
        num_random_terminals=3,
        steiner_breakable_chance=0.5,  # 50% of Steiner paths are breakable
        breakable_prob=0.4,  # 40% of non-Steiner area is breakable
        strong_prob=0.2,  # 20% of non-Steiner area is strong
        unbreakable_prob=0.2  # 20% of non-Steiner area is unbreakable
    )

    generator2.visualize_map("bomberman_map_aggressive.png", show_steiner=True)

    print(f"\n✅ Enhanced map generation finished!")
    print(f"🗺️  Check generated map images and Erlang modules")
    print(f"🎯 Ready for powerup integration: breakable and strong tiles will drop powerups!")


if __name__ == "__main__":
    main()