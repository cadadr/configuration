// i3-appropriate-layout.c --- match layouts of active workspaces to screen orientation

#include <stdlib.h>
#include <string.h>

#include <glib/gprintf.h>
#include <i3ipc-glib/i3ipc-glib.h>

#include <xcb/xcb.h>
#include <xcb/randr.h>

enum orientation
  {
    Vertical,
    Horizontal
  };

void
i3command(const gchar *command, i3ipcCon *ws)
{
  GError *err = NULL;

  i3ipc_con_command(ws, command, &err);

  if(err)
    {
      g_printf("i3 IPC command error: %s\n", err->message);
      exit(EXIT_FAILURE);
    }
}

void
maybe_set_layout(gpointer data, gpointer user_data)
{
  i3ipcCon *ws = data;
  enum orientation *orientation = user_data;
  gchar *layout;

  g_object_get(ws, "layout", &layout, NULL);

  if (strcmp(layout, "splitv") && (*orientation) == Horizontal)
    {
      i3command("layout splith", ws);
    }
  else if (strcmp(layout, "splith") && (*orientation) == Vertical)
    {
      i3command("layout splitv", ws);
    }
}

enum orientation
get_display_orientation(void)
{
  xcb_connection_t *connection;
  const xcb_setup_t *setup;
  xcb_screen_iterator_t iter;
  xcb_screen_t *screen;
  xcb_window_t root;
  xcb_randr_get_screen_info_cookie_t cookie;
  xcb_generic_error_t *error;
  xcb_randr_get_screen_info_reply_t *reply;
  uint16_t rotation;
  connection = xcb_connect(NULL, NULL);
  setup = xcb_get_setup(connection);

  // Get first screen.
  iter = xcb_setup_roots_iterator(setup);
  screen = iter.data;
  root = screen->root;

  cookie = xcb_randr_get_screen_info(connection, root);
  reply = xcb_randr_get_screen_info_reply(connection, cookie, &error);

  if (error)
    {
      printf("xcb error: code %d\n", error->error_code);
      exit(EXIT_FAILURE);
    }
  else
    {
      rotation = reply->rotation;
      switch (rotation)
        {
        case 010:
        case 01000:
          return Vertical;
        default:
          return Horizontal;
        }
    }
}

gint
main(const int argc, const char **argv)
{
  i3ipcConnection *connection;
  i3ipcCon *tree;
  GList *workspaces;
  enum orientation orientation;

  orientation = get_display_orientation();
  connection = i3ipc_connection_new(NULL, NULL);
  tree = i3ipc_connection_get_tree(connection, NULL);
  workspaces = i3ipc_con_workspaces(tree);

  g_list_foreach(workspaces, maybe_set_layout, &orientation);

  exit(EXIT_SUCCESS);
}
