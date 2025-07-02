import pygame
import sys
import os

# Redirect stdout for Erlang communication, and suppress pygame startup output
sys.stdout = open(sys.stdout.fileno(), mode='w', buffering=1, encoding='utf-8', errors='replace')
# sys.stderr = open(os.devnull, 'w')

# Initialize Pygame
pygame.init()
SCREEN_WIDTH, SCREEN_HEIGHT = 800, 600
screen = pygame.display.set_mode((SCREEN_WIDTH, SCREEN_HEIGHT))
pygame.display.set_caption("Playing With Fire 2")

# Colors
WHITE = (255, 255, 255)
BLACK = (0, 0, 0)
GRAY = (160, 160, 160)
RED = (200, 50, 50)
BG_COLOR = (78, 53, 36)  # Warm brown-ish background for consistency

# Fonts
font = pygame.font.SysFont("arial", 48)
title_font = pygame.font.SysFont("impact", 80)

# Global state
current_screen = "main_menu"
dot_count = 1
dot_timer = pygame.time.get_ticks()

# Load images
logo_img = pygame.image.load("assets/logo.png").convert_alpha()
bomb_img = pygame.image.load("assets/bomb.png").convert_alpha()

def scale_image_preserve_ratio(image, max_width=None, max_height=None):
    original_width, original_height = image.get_size()
    aspect_ratio = original_width / original_height

    if max_width and not max_height:
        width = max_width
        height = int(width / aspect_ratio)
    elif max_height and not max_width:
        height = max_height
        width = int(height * aspect_ratio)
    elif max_width and max_height:
        width = min(max_width, int(max_height * aspect_ratio))
        height = min(max_height, int(max_width / aspect_ratio))
    else:
        width, height = original_width, original_height

    return pygame.transform.smoothscale(image, (width, height))

def draw_text(text, y, center=True, size=48, color=BLACK):
    fnt = pygame.font.SysFont(None, size)
    surface = fnt.render(text, True, color)
    rect = surface.get_rect(center=(SCREEN_WIDTH // 2, y) if center else (20, y))
    screen.blit(surface, rect)

def draw_screen_template(title, title_y=100):
    screen.fill(BG_COLOR)
    title_surface = title_font.render(title, True, BLACK)
    title_rect = title_surface.get_rect(center=(SCREEN_WIDTH // 2, title_y))
    screen.blit(title_surface, title_rect)

BUTTON_COLOR = (240, 120, 60)        # Warm orange for buttons
BUTTON_HOVER = (255, 160, 100)       # Lighter orange for hover effect
TEXT_COLOR = (30, 30, 30)            # Dark text color for contrast

def draw_button(text, y, mouse_pos):
    rect = pygame.Rect(0, 0, 300, 60)
    rect.center = (SCREEN_WIDTH // 2, y)
    color = BUTTON_HOVER if rect.collidepoint(mouse_pos) else BUTTON_COLOR
    pygame.draw.rect(screen, color, rect, border_radius=10)
    draw_text(text, y, size=36, color=TEXT_COLOR)
    return rect

def draw_play_button_with_icon(y, mouse_pos):
    rect = pygame.Rect(0, 0, 300, 60)
    rect.center = (SCREEN_WIDTH // 2, y)
    color = BUTTON_HOVER if rect.collidepoint(mouse_pos) else BUTTON_COLOR
    pygame.draw.rect(screen, color, rect, border_radius=10)

    # icon + text surfaces
    icon = scale_image_preserve_ratio(bomb_img, max_height=36)
    fnt = pygame.font.SysFont(None, 36)
    text_surface = fnt.render("Play", True, TEXT_COLOR)

    total_width = icon.get_width() + 10 + text_surface.get_width()  

    icon_x = rect.centerx - total_width // 2 - 15
    text_x = rect.centerx - 25

    icon_rect = icon.get_rect()
    icon_rect.left = icon_x
    icon_rect.centery = y

    text_rect = text_surface.get_rect()
    text_rect.left = text_x
    text_rect.centery = y

    screen.blit(icon, icon_rect)
    screen.blit(text_surface, text_rect)

    return rect


def show_main_menu(mouse_pos):
    screen.fill(BG_COLOR)

    logo_scaled = scale_image_preserve_ratio(logo_img, max_width=400)
    logo_rect = logo_scaled.get_rect(center=(SCREEN_WIDTH // 2, 120))
    screen.blit(logo_scaled, logo_rect)
    
    play_btn = draw_play_button_with_icon(300, mouse_pos) 
    exit_btn = draw_button("Exit", 390, mouse_pos)
    return [("play_clicked", play_btn), ("exit_clicked", exit_btn)]

def show_loading():
    global dot_count, dot_timer
    draw_screen_template("")

    # Animate dots every 500ms
    if pygame.time.get_ticks() - dot_timer > 500:
        dot_count = (dot_count % 3) + 1
        dot_timer = pygame.time.get_ticks()

    draw_text("Connecting to server" + "." * dot_count, SCREEN_HEIGHT // 2, size=40)

def show_error(mouse_pos):
    draw_screen_template("Failed To Connect")
    retry_btn = draw_button("Retry", 300, mouse_pos)
    return_btn = draw_button("Return To Main Menu", 380, mouse_pos)
    return [("retry_clicked", retry_btn), ("return_to_menu", return_btn)]

def send_event(event_str):
    try:
        sys.stdout.write(event_str + "\n")
        sys.stdout.flush()
    except BrokenPipeError:
        pygame.quit()
        return

def listen_for_commands():
    import threading
    def reader():
        global current_screen
        for line in sys.stdin:
            cmd = line.strip()
            if cmd in {"show_main_menu", "show_loading", "show_error"}:
                current_screen = cmd
    threading.Thread(target=reader, daemon=True).start()

def main():
    global current_screen
    listen_for_commands()
    current_screen = "show_main_menu"

    clock = pygame.time.Clock()
    buttons = []

    running = True
    while running:
        mouse_pos = pygame.mouse.get_pos()
        screen.fill(BG_COLOR)

        if current_screen == "show_main_menu":
            buttons = show_main_menu(mouse_pos)
        elif current_screen == "show_loading":
            show_loading()
            buttons = []
        elif current_screen == "show_error":
            buttons = show_error(mouse_pos)
        else:
            draw_text("Waiting for command...", SCREEN_HEIGHT // 2)
            buttons = []

        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                pygame.quit()
                running = False

            elif event.type == pygame.MOUSEBUTTONDOWN and event.button == 1:
                for label, rect in buttons:
                    if rect.collidepoint(event.pos):
                        send_event(label)
                        if label == "exit_clicked":
                            pygame.quit()
                            running = False

        pygame.display.flip()
        clock.tick(60)

if __name__ == "__main__":
    main()
