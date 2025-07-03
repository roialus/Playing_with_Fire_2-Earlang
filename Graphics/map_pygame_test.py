import pygame
import sys
import math

# Initialize Pygame
pygame.init()

# Constants
TILE_SIZE = 40
MAP_SIZE = 16
WINDOW_WIDTH = MAP_SIZE * TILE_SIZE
WINDOW_HEIGHT = MAP_SIZE * TILE_SIZE + 120  # Extra space for info

# Colors
COLORS = {
    'FREE': (200, 200, 200),  # Light gray floor
    'BACKGROUND': (50, 50, 50),  # Dark background
    'TEXT': (255, 255, 255),  # White text
    'BORDER': (0, 0, 0),  # Black borders

    # Tile colors
    'BRICK': (139, 69, 19),  # Brown brick
    'BRICK_DARK': (101, 50, 14),  # Darker brown for mortar
    'WOOD': (160, 82, 45),  # Wood barrel
    'WOOD_DARK': (120, 60, 30),  # Darker wood
    'METAL': (105, 105, 105),  # Metal barrel
    'METAL_DARK': (70, 70, 70),  # Darker metal
    # Player outfit colors
    'PLAYER_1': (0, 100, 255),  # Blue
    'PLAYER_2': (255, 50, 50),  # Red
    'PLAYER_3': (50, 200, 50),  # Green
    'PLAYER_4': (255, 200, 0),  # Yellow
    'SKIN': (255, 224, 189),  # Light skin tone

    # Power-up colors
    'SHOE': (255, 215, 0),  # Gold shoe
    'REMOTE': (64, 64, 64),  # Dark remote
    'BOMB': (64, 64, 64),  # Dark bomb
    'HEART': (255, 0, 0),  # Red heart
    'GHOST': (240, 248, 255),  # Ghost white
    'ICE': (173, 216, 230),  # Light blue ice
    'EXPLOSION': (255, 165, 0),  # Orange explosion
    'PLUS': (70, 70, 70) # Silver plus sign
}

# Tile type mappings
TILE_TYPES = {
    0: 'FREE',
    1: 'BREAKABLE',
    2: 'UNBREAKABLE',
    3: 'STRONG',
    4: 'PLAYER_START'
}


class MapVisualizerSprites:
    def __init__(self):
        self.screen = pygame.display.set_mode((WINDOW_WIDTH, WINDOW_HEIGHT))
        pygame.display.set_caption("Bomberman Map Visualizer - Sprites")
        self.clock = pygame.time.Clock()
        self.font = pygame.font.Font(None, 24)
        self.small_font = pygame.font.Font(None, 16)

        # Load map data (converted from your Erlang export)
        self.map_data = self.load_test_data()

    def load_test_data(self):
        """Load the map data converted from your Erlang export"""
        tiles = [
            [2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2],
            [2, 4, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 2],
            [2, 0, 0, 0, 1, 1, 0, 1, 0, 3, 1, 0, 3, 0, 0, 2],
            [2, 0, 1, 2, 1, 2, 2, 3, 1, 1, 0, 3, 1, 0, 0, 2],
            [2, 2, 1, 3, 0, 1, 3, 1, 3, 3, 2, 0, 1, 1, 0, 2],
            [2, 2, 0, 1, 1, 1, 1, 1, 1, 0, 1, 3, 0, 0, 0, 2],
            [2, 0, 0, 0, 2, 3, 1, 0, 1, 3, 3, 1, 2, 0, 0, 2],
            [2, 1, 0, 0, 2, 0, 1, 1, 1, 1, 3, 1, 1, 1, 0, 2],
            [2, 0, 3, 0, 0, 1, 1, 1, 1, 2, 3, 0, 0, 0, 0, 2],
            [2, 0, 3, 0, 1, 1, 2, 1, 0, 1, 3, 1, 0, 0, 0, 2],
            [2, 2, 1, 2, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2],
            [2, 0, 1, 2, 2, 0, 1, 1, 1, 2, 0, 3, 0, 0, 1, 2],
            [2, 3, 1, 2, 0, 3, 3, 0, 2, 1, 0, 1, 0, 1, 1, 2],
            [2, 0, 0, 1, 1, 0, 0, 0, 0, 1, 3, 2, 0, 0, 0, 2],
            [2, 4, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 4, 2],
            [2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2]
        ]

        powerups = [
            ["none", "none", "none", "none", "none", "none", "none", "none", "none", "none", "none", "none", "none",
             "none", "none", "none"],
            ["none", "none", "none", "plus_life", "none", "none", "none", "none", "none", "none", "none", "none",
             "none", "none", "none", "none"],
            ["none", "none", "none", "none", "freeze_bomb", "none", "none", "plus_bombs", "none", "move_speed",
             "plus_life", "none", "phased", "none", "none", "none"],
            ["none", "none", "none", "none", "plus_life", "none", "none", "plus_life", "none", "none", "none",
             "bigger_explosion", "none", "none", "none", "none"],
            ["none", "none", "none", "repeat_bombs", "none", "none", "bigger_explosion", "none", "remote_ignition",
             "bigger_explosion", "none", "none", "bigger_explosion", "none", "none", "none"],
            ["none", "none", "none", "kick_bomb", "remote_ignition", "none", "none", "plus_bombs", "none", "none",
             "none", "remote_ignition", "none", "none", "none", "none"],
            ["none", "none", "none", "none", "none", "bigger_explosion", "none", "none", "plus_life", "move_speed",
             "plus_bombs", "kick_bomb", "none", "none", "none", "none"],
            ["none", "plus_life", "none", "none", "none", "none", "none", "kick_bomb", "plus_bombs", "plus_life",
             "kick_bomb", "move_speed", "remote_ignition", "freeze_bomb", "none", "none"],
            ["none", "none", "plus_life", "none", "none", "repeat_bombs", "none", "plus_bombs", "none", "none",
             "plus_bombs", "none", "none", "none", "none", "none"],
            ["none", "none", "move_speed", "none", "none", "plus_life", "none", "move_speed", "none", "none",
             "kick_bomb", "none", "none", "none", "none", "none"],
            ["none", "none", "remote_ignition", "none", "none", "repeat_bombs", "none", "none", "none", "none", "none",
             "none", "none", "none", "none", "none"],
            ["none", "none", "none", "none", "none", "none", "bigger_explosion", "none", "plus_bombs", "none", "none",
             "bigger_explosion", "none", "none", "plus_life", "none"],
            ["none", "remote_ignition", "remote_ignition", "none", "none", "bigger_explosion", "freeze_bomb", "none",
             "none", "none", "none", "plus_life", "none", "none", "none", "none"],
            ["none", "none", "none", "none", "bigger_explosion", "none", "none", "none", "none", "remote_ignition",
             "plus_life", "none", "none", "none", "none", "none"],
            ["none", "none", "none", "none", "none", "none", "none", "kick_bomb", "none", "phased", "remote_ignition",
             "none", "none", "none", "none", "none"],
            ["none", "none", "none", "none", "none", "none", "none", "none", "none", "none", "none", "none", "none",
             "none", "none", "none"]
        ]

        return {
            'tiles': tiles,
            'powerups': powerups,
            'player_starts': [
                {'player': 1, 'x': 1, 'y': 1},
                {'player': 2, 'x': 1, 'y': 14},
                {'player': 3, 'x': 14, 'y': 1},
                {'player': 4, 'x': 14, 'y': 14}
            ]
        }

    def draw_brick_wall(self, x, y):
        """Draw brick wall sprite"""
        # Draw brick pattern
        pygame.draw.rect(self.screen, COLORS['BRICK'], (x, y, TILE_SIZE, TILE_SIZE))

        # Draw brick lines
        brick_h = TILE_SIZE // 4
        for row in range(4):
            brick_y = y + row * brick_h
            # Horizontal lines
            pygame.draw.line(self.screen, COLORS['BRICK_DARK'], (x, brick_y), (x + TILE_SIZE, brick_y), 1)

            # Vertical lines (offset every other row)
            offset = (TILE_SIZE // 3) if row % 2 == 0 else 0
            for i in range(3):
                brick_x = x + offset + i * (TILE_SIZE // 3)
                if brick_x < x + TILE_SIZE:
                    pygame.draw.line(self.screen, COLORS['BRICK_DARK'], (brick_x, brick_y),
                                     (brick_x, brick_y + brick_h), 1)

    def draw_wooden_barrel(self, x, y):
        """Draw wooden barrel sprite"""
        # Barrel body
        pygame.draw.rect(self.screen, COLORS['WOOD'], (x + 5, y + 3, TILE_SIZE - 10, TILE_SIZE - 6))

        # Barrel bands
        pygame.draw.rect(self.screen, COLORS['WOOD_DARK'], (x + 3, y + 8, TILE_SIZE - 6, 3))
        pygame.draw.rect(self.screen, COLORS['WOOD_DARK'], (x + 3, y + 15, TILE_SIZE - 6, 3))
        pygame.draw.rect(self.screen, COLORS['WOOD_DARK'], (x + 3, y + 25, TILE_SIZE - 6, 3))

        # Wood grain lines
        for i in range(5):
            line_x = x + 8 + i * 6
            pygame.draw.line(self.screen, COLORS['WOOD_DARK'], (line_x, y + 5), (line_x, y + TILE_SIZE - 5), 1)

    def draw_metal_barrel(self, x, y):
        """Draw metal barrel sprite"""
        # Barrel body
        pygame.draw.rect(self.screen, COLORS['METAL'], (x + 5, y + 3, TILE_SIZE - 10, TILE_SIZE - 6))

        # Metal bands
        pygame.draw.rect(self.screen, COLORS['METAL_DARK'], (x + 3, y + 8, TILE_SIZE - 6, 4))
        pygame.draw.rect(self.screen, COLORS['METAL_DARK'], (x + 3, y + 18, TILE_SIZE - 6, 4))
        pygame.draw.rect(self.screen, COLORS['METAL_DARK'], (x + 3, y + 28, TILE_SIZE - 6, 4))

        # Metal shine
        pygame.draw.line(self.screen, (180, 180, 180), (x + 10, y + 5), (x + 10, y + TILE_SIZE - 5), 2)

    def draw_running_shoe(self, x, y):
        """Draw running shoe sprite"""
        center_x, center_y = x + TILE_SIZE // 2, y + TILE_SIZE // 2

        # Shoe sole
        pygame.draw.ellipse(self.screen, COLORS['SHOE'], (center_x - 12, center_y - 6, 24, 12))

        # Shoe upper
        pygame.draw.ellipse(self.screen, (100, 100, 100), (center_x - 10, center_y - 10, 20, 10))

        # Speed lines
        for i in range(3):
            line_x = center_x + 15 + i * 3
            pygame.draw.line(self.screen, (200, 200, 200), (line_x, center_y - 3), (line_x + 5, center_y - 3), 2)

    def draw_remote_control(self, x, y):
        """Draw remote control sprite"""
        center_x, center_y = x + TILE_SIZE // 2, y + TILE_SIZE // 2

        # Remote body
        pygame.draw.rect(self.screen, COLORS['REMOTE'], (center_x - 8, center_y - 12, 16, 24))
        pygame.draw.rect(self.screen, (100, 100, 100), (center_x - 8, center_y - 12, 16, 24), 2)

        # Buttons
        pygame.draw.circle(self.screen, (200, 0, 0), (center_x - 3, center_y - 6), 3)
        pygame.draw.circle(self.screen, (200, 200, 200), (center_x + 3, center_y - 6), 3)
        pygame.draw.rect(self.screen, (0, 200, 0), (center_x - 4, center_y + 2, 8, 4))

        # Antenna
        pygame.draw.line(self.screen, (150, 150, 150), (center_x, center_y - 12), (center_x - 3, center_y - 18), 2)

    def draw_repeat_bombs(self, x, y):
        """Draw repeat bombs (big bomb + small bomb)"""
        center_x, center_y = x + TILE_SIZE // 2, y + TILE_SIZE // 2

        # Large bomb
        pygame.draw.circle(self.screen, COLORS['BOMB'], (center_x - 4, center_y), 10)
        pygame.draw.circle(self.screen, (40, 40, 40), (center_x - 4, center_y), 10, 2)

        # Large bomb fuse
        pygame.draw.line(self.screen, (139, 69, 19), (center_x - 4, center_y - 10), (center_x - 6, center_y - 15), 2)
        pygame.draw.circle(self.screen, (255, 165, 0), (center_x - 6, center_y - 15), 2)

        # Small bomb
        pygame.draw.circle(self.screen, COLORS['BOMB'], (center_x + 8, center_y + 6), 6)
        pygame.draw.circle(self.screen, (40, 40, 40), (center_x + 8, center_y + 6), 6, 1)

        # Small bomb fuse
        pygame.draw.line(self.screen, (139, 69, 19), (center_x + 8, center_y), (center_x + 10, center_y - 3), 1)

    def draw_kick_emoji(self, x, y):
        """Draw kick emoji (boot)"""
        center_x, center_y = x + TILE_SIZE // 2, y + TILE_SIZE // 2

        # Boot sole
        pygame.draw.ellipse(self.screen, (101, 67, 33), (center_x - 12, center_y + 2, 20, 8))

        # Boot upper
        pygame.draw.rect(self.screen, (139, 69, 19), (center_x - 8, center_y - 8, 12, 12))

        # Boot top
        pygame.draw.rect(self.screen, (160, 82, 45), (center_x - 6, center_y - 12, 8, 6))

        # Motion lines
        for i in range(3):
            line_y = center_y - 5 + i * 3
            pygame.draw.line(self.screen, (200, 200, 200), (center_x + 10, line_y), (center_x + 16, line_y), 2)

    def draw_ghost(self, x, y):
        """Draw ghost sprite"""
        center_x, center_y = x + TILE_SIZE // 2, y + TILE_SIZE // 2

        # Ghost body
        pygame.draw.circle(self.screen, COLORS['GHOST'], (center_x, center_y - 2), 12)
        pygame.draw.rect(self.screen, COLORS['GHOST'], (center_x - 12, center_y - 2, 24, 12))

        # Ghost bottom wavy edge
        for i in range(6):
            wave_x = center_x - 12 + i * 4
            wave_y = center_y + 10 if i % 2 == 0 else center_y + 6
            if i < 5:
                next_x = center_x - 12 + (i + 1) * 4
                next_y = center_y + 6 if i % 2 == 0 else center_y + 10
                pygame.draw.line(self.screen, COLORS['GHOST'], (wave_x, wave_y), (next_x, next_y), 3)

        # Ghost eyes
        pygame.draw.circle(self.screen, (0, 0, 0), (center_x - 4, center_y - 4), 2)
        pygame.draw.circle(self.screen, (0, 0, 0), (center_x + 4, center_y - 4), 2)

        # Ghost outline
        pygame.draw.circle(self.screen, (200, 200, 200), (center_x, center_y - 2), 12, 2)

    def draw_bomb_with_plus(self, x, y):
        """Draw bomb with plus sign"""
        center_x, center_y = x + TILE_SIZE // 2, y + TILE_SIZE // 2

        # Bomb
        pygame.draw.circle(self.screen, COLORS['BOMB'], (center_x - 3, center_y), 10)
        pygame.draw.circle(self.screen, (40, 40, 40), (center_x - 3, center_y), 10, 2)

        # Fuse
        pygame.draw.line(self.screen, (139, 69, 19), (center_x - 3, center_y - 10), (center_x - 5, center_y - 15), 2)
        pygame.draw.circle(self.screen, (255, 165, 0), (center_x - 5, center_y - 15), 2)

        # Plus sign
        pygame.draw.line(self.screen, COLORS['PLUS'], (center_x + 8, center_y - 4), (center_x + 8, center_y + 4), 3)
        pygame.draw.line(self.screen, COLORS['PLUS'], (center_x + 4, center_y), (center_x + 12, center_y), 3)

    def draw_explosion_with_plus(self, x, y):
        """Draw explosion with plus sign"""
        center_x, center_y = x + TILE_SIZE // 2, y + TILE_SIZE // 2

        # Explosion burst
        pygame.draw.circle(self.screen, COLORS['EXPLOSION'], (center_x - 3, center_y), 8)

        # Explosion spikes
        for angle in range(0, 360, 45):
            rad = math.radians(angle)
            end_x = center_x - 3 + int(12 * math.cos(rad))
            end_y = center_y + int(12 * math.sin(rad))
            pygame.draw.line(self.screen, COLORS['EXPLOSION'], (center_x - 3, center_y), (end_x, end_y), 2)

        # Plus sign
        pygame.draw.line(self.screen, COLORS['PLUS'], (center_x + 8, center_y - 4), (center_x + 8, center_y + 4), 3)
        pygame.draw.line(self.screen, COLORS['PLUS'], (center_x + 4, center_y), (center_x + 12, center_y), 3)

    def draw_heart_with_plus(self, x, y):
        """Draw heart with plus sign"""
        center_x, center_y = x + TILE_SIZE // 2, y + TILE_SIZE // 2

        # Heart (two circles and triangle)
        pygame.draw.circle(self.screen, COLORS['HEART'], (center_x - 6, center_y - 2), 6)
        pygame.draw.circle(self.screen, COLORS['HEART'], (center_x + 2, center_y - 2), 6)

        # Heart bottom point
        points = [(center_x - 8, center_y + 2), (center_x + 4, center_y + 2), (center_x - 2, center_y + 10)]
        pygame.draw.polygon(self.screen, COLORS['HEART'], points)

        # Plus sign
        pygame.draw.line(self.screen, COLORS['PLUS'], (center_x + 10, center_y - 4), (center_x + 10, center_y + 4), 3)
        pygame.draw.line(self.screen, COLORS['PLUS'], (center_x + 6, center_y), (center_x + 14, center_y), 3)

    def draw_ice_cube(self, x, y):
        """Draw ice cube sprite"""
        center_x, center_y = x + TILE_SIZE // 2, y + TILE_SIZE // 2

        # Ice cube
        pygame.draw.rect(self.screen, COLORS['ICE'], (center_x - 10, center_y - 10, 20, 20))
        pygame.draw.rect(self.screen, (100, 149, 237), (center_x - 10, center_y - 10, 20, 20), 2)

        # Ice shine/reflection lines
        pygame.draw.line(self.screen, (255, 255, 255), (center_x - 7, center_y - 7), (center_x - 3, center_y - 7), 2)
        pygame.draw.line(self.screen, (255, 255, 255), (center_x - 7, center_y - 3), (center_x - 7, center_y + 1), 2)

        # Ice crystals
        for i in range(3):
            crystal_x = center_x - 3 + i * 3
            crystal_y = center_y + 2 + (i % 2) * 3
            pygame.draw.line(self.screen, (255, 255, 255), (crystal_x, crystal_y - 2), (crystal_x, crystal_y + 2), 1)
            pygame.draw.line(self.screen, (255, 255, 255), (crystal_x - 2, crystal_y), (crystal_x + 2, crystal_y), 1)

    def draw_powerup(self, x, y, powerup):
        """Draw powerup sprite based on type"""
        if powerup == "move_speed":
            self.draw_running_shoe(x, y)
        elif powerup == "remote_ignition":
            self.draw_remote_control(x, y)
        elif powerup == "repeat_bombs":
            self.draw_repeat_bombs(x, y)
        elif powerup == "kick_bomb":
            self.draw_kick_emoji(x, y)
        elif powerup == "phased":
            self.draw_ghost(x, y)
        elif powerup == "plus_bombs":
            self.draw_bomb_with_plus(x, y)
        elif powerup == "bigger_explosion":
            self.draw_explosion_with_plus(x, y)
        elif powerup == "plus_life":
            self.draw_heart_with_plus(x, y)
        elif powerup == "freeze_bomb":
            self.draw_ice_cube(x, y)

    def draw_player_character(self, x, y, player_num):
        """Draw a kid character with hat in different colored outfit"""
        center_x = x + TILE_SIZE // 2
        center_y = y + TILE_SIZE // 2

        # Get player color
        player_colors = {
            1: COLORS['PLAYER_1'],  # Blue
            2: COLORS['PLAYER_2'],  # Red
            3: COLORS['PLAYER_3'],  # Green
            4: COLORS['PLAYER_4']  # Yellow
        }
        outfit_color = player_colors.get(player_num, COLORS['PLAYER_1'])

        # Draw head (skin color)
        pygame.draw.circle(self.screen, COLORS['SKIN'], (center_x, center_y - 8), 8)
        pygame.draw.circle(self.screen, COLORS['BORDER'], (center_x, center_y - 8), 8, 1)

        # Draw Head
        # Head base
        pygame.draw.circle(self.screen, COLORS['SKIN'], (center_x, center_y - 12), 10)

        # Draw eyes
        pygame.draw.circle(self.screen, COLORS['BORDER'], (center_x - 3, center_y - 10), 1)
        pygame.draw.circle(self.screen, COLORS['BORDER'], (center_x + 3, center_y - 10), 1)

        # Draw nose (tiny dot)
        pygame.draw.circle(self.screen, (255, 180, 140), (center_x, center_y - 7), 1)

        # Draw mouth (small curve)
        pygame.draw.arc(self.screen, COLORS['BORDER'], (center_x - 3, center_y - 6, 6, 4), 0, 3.14, 1)

        # Draw body (outfit color)
        pygame.draw.rect(self.screen, outfit_color, (center_x - 6, center_y, 12, 14))
        pygame.draw.rect(self.screen, COLORS['BORDER'], (center_x - 6, center_y, 12, 14), 1)

        # Draw arms
        pygame.draw.circle(self.screen, COLORS['SKIN'], (center_x - 10, center_y + 4), 3)
        pygame.draw.circle(self.screen, COLORS['SKIN'], (center_x + 10, center_y + 4), 3)
        pygame.draw.rect(self.screen, outfit_color, (center_x - 12, center_y + 2, 4, 8))
        pygame.draw.rect(self.screen, outfit_color, (center_x + 8, center_y + 2, 4, 8))

        # Draw legs
        pygame.draw.rect(self.screen, outfit_color, (center_x - 4, center_y + 14, 3, 8))
        pygame.draw.rect(self.screen, outfit_color, (center_x + 1, center_y + 14, 3, 8))

        # Draw shoes (dark)
        pygame.draw.ellipse(self.screen, (50, 50, 50), (center_x - 5, center_y + 20, 4, 3))
        pygame.draw.ellipse(self.screen, (50, 50, 50), (center_x + 1, center_y + 20, 4, 3))

        # # Draw player number on outfit
        # text = self.small_font.render(str(player_num), True, COLORS['TEXT'])
        # text_rect = text.get_rect(center=(center_x, center_y + 7))
        # self.screen.blit(text, text_rect)
        # """Draw powerup sprite based on type"""
        # if powerup == "move_speed":
        #     self.draw_running_shoe(x, y)
        # elif powerup == "remote_ignition":
        #     self.draw_remote_control(x, y)
        # elif powerup == "repeat_bombs":
        #     self.draw_repeat_bombs(x, y)
        # elif powerup == "kick_bomb":
        #     self.draw_kick_emoji(x, y)
        # elif powerup == "phased":
        #     self.draw_ghost(x, y)
        # elif powerup == "plus_bombs":
        #     self.draw_bomb_with_plus(x, y)
        # elif powerup == "bigger_explosion":
        #     self.draw_explosion_with_plus(x, y)
        # elif powerup == "plus_life":
        #     self.draw_heart_with_plus(x, y)
        # elif powerup == "freeze_bomb":
        #     self.draw_ice_cube(x, y)

    def draw_tile(self, x, y, tile_type, powerup):
        """Draw a single tile with its powerup if any"""
        pixel_x = y * TILE_SIZE
        pixel_y = x * TILE_SIZE

        # Draw base tile
        if tile_type == 0:  # FREE
            pygame.draw.rect(self.screen, COLORS['FREE'], (pixel_x, pixel_y, TILE_SIZE, TILE_SIZE))
        elif tile_type == 1:  # BREAKABLE - wooden barrel
            pygame.draw.rect(self.screen, COLORS['FREE'], (pixel_x, pixel_y, TILE_SIZE, TILE_SIZE))
            self.draw_wooden_barrel(pixel_x, pixel_y)
        elif tile_type == 2:  # UNBREAKABLE - brick wall
            self.draw_brick_wall(pixel_x, pixel_y)
        elif tile_type == 3:  # STRONG - metal barrel
            pygame.draw.rect(self.screen, COLORS['FREE'], (pixel_x, pixel_y, TILE_SIZE, TILE_SIZE))
            self.draw_metal_barrel(pixel_x, pixel_y)
        elif tile_type == 4:  # PLAYER_START
            pygame.draw.rect(self.screen, (200, 255, 200), (pixel_x, pixel_y, TILE_SIZE, TILE_SIZE))

        # Draw border
        pygame.draw.rect(self.screen, COLORS['BORDER'], (pixel_x, pixel_y, TILE_SIZE, TILE_SIZE), 1)

        # Draw powerup if present and tile is breakable/strong
        # if powerup != "none" and tile_type in [1, 3]:
        #     # Draw semi-transparent overlay to show powerup is hidden
        #     overlay = pygame.Surface((TILE_SIZE, TILE_SIZE))
        #     overlay.set_alpha(100)
        #     overlay.fill((255, 255, 0))  # Yellow tint to show powerup
        #     self.screen.blit(overlay, (pixel_x, pixel_y))

    def draw_player_starts(self):
        """Draw player character sprites at start positions"""
        for player in self.map_data['player_starts']:
            x, y = player['x'], player['y']
            pixel_x = y * TILE_SIZE
            pixel_y = x * TILE_SIZE

            # Draw the kid character
            self.draw_player_character(pixel_x, pixel_y, player['player'])

    def draw_powerup_showcase(self):
        """Draw a showcase of all powerups at the bottom"""
        showcase_y = MAP_SIZE * TILE_SIZE + 50
        powerup_types = [
            "move_speed", "remote_ignition", "repeat_bombs", "kick_bomb", "phased",
            "plus_bombs", "bigger_explosion", "plus_life", "freeze_bomb"
        ]

        powerup_names = [
            "Speed", "Remote", "Repeat", "Kick", "Ghost",
            "+Bomb", "+Expl", "+Life", "Freeze"
        ]

        for i, (powerup, name) in enumerate(zip(powerup_types, powerup_names)):
            showcase_x = 10 + i * 70

            # Draw powerup
            pygame.draw.rect(self.screen, COLORS['FREE'], (showcase_x, showcase_y, 40, 40))
            pygame.draw.rect(self.screen, COLORS['BORDER'], (showcase_x, showcase_y, 40, 40), 1)
            self.draw_powerup(showcase_x, showcase_y, powerup)

            # Draw name
            text = self.small_font.render(name, True, COLORS['TEXT'])
            text_rect = text.get_rect(center=(showcase_x + 20, showcase_y + 50))
            self.screen.blit(text, text_rect)

    def draw_info_panel(self):
        """Draw information panel at the bottom"""
        info_y = MAP_SIZE * TILE_SIZE
        pygame.draw.rect(self.screen, COLORS['BACKGROUND'], (0, info_y, WINDOW_WIDTH, 120))

        # Instructions
        instructions = [
            "🎮 Bomberman Map - Wood=Breakable, Metal=Strong, Brick=Walls | Players: Blue, Red, Green, Yellow",
            "💎 Yellow tint on barrels shows hidden power-ups | Click tiles to inspect | ESC to exit"
        ]

        for i, text in enumerate(instructions):
            rendered = self.small_font.render(text, True, COLORS['TEXT'])
            self.screen.blit(rendered, (10, info_y + 10 + i * 15))

        # Draw powerup showcase
        self.draw_powerup_showcase()

    def draw_map(self):
        """Draw the complete map"""
        self.screen.fill(COLORS['BACKGROUND'])

        # Draw all tiles
        for x in range(MAP_SIZE):
            for y in range(MAP_SIZE):
                tile_type = self.map_data['tiles'][x][y]
                powerup = self.map_data['powerups'][x][y]
                self.draw_tile(x, y, tile_type, powerup)

        # Draw player starts
        self.draw_player_starts()

        # Draw info panel
        self.draw_info_panel()

    def handle_events(self):
        """Handle pygame events"""
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                return False
            elif event.type == pygame.KEYDOWN:
                if event.key == pygame.K_ESCAPE:
                    return False
            elif event.type == pygame.MOUSEBUTTONDOWN:
                # Click to inspect tile
                mouse_x, mouse_y = pygame.mouse.get_pos()
                if mouse_y < MAP_SIZE * TILE_SIZE:  # Within map area
                    tile_x = mouse_y // TILE_SIZE
                    tile_y = mouse_x // TILE_SIZE
                    if 0 <= tile_x < MAP_SIZE and 0 <= tile_y < MAP_SIZE:
                        tile_type = self.map_data['tiles'][tile_x][tile_y]
                        powerup = self.map_data['powerups'][tile_x][tile_y]
                        tile_name = TILE_TYPES.get(tile_type, 'UNKNOWN')
                        print(f"🔍 Clicked ({tile_x}, {tile_y}): {tile_name} tile with {powerup} powerup")
        return True

    def run(self):
        """Main game loop"""
        print("🗺️  Bomberman Map Visualizer with Kid Characters Started!")
        print("👦 Players: Blue, Red, Green, Yellow kids with hats")
        print("🔍 Click on tiles to see their properties")
        print("💎 Yellow tint shows barrels with hidden power-ups")
        print("🎮 Press ESC or close window to exit")

        running = True
        while running:
            running = self.handle_events()

            self.draw_map()
            pygame.display.flip()
            self.clock.tick(60)

        pygame.quit()
        sys.exit()


if __name__ == "__main__":
    visualizer = MapVisualizerSprites()
    visualizer.run()
