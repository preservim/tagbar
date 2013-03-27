public class GnoMine : Gtk.Application
{
    /* Settings keys */
    private Settings settings;
    private const string KEY_XSIZE = "xsize";
    private const int XSIZE_MIN = 4;
    private const int XSIZE_MAX = 100;
    private const string KEY_YSIZE = "ysize";
    private const int YSIZE_MIN = 4;
    private const int YSIZE_MAX = 100;
    private const string KEY_NMINES = "nmines";
    private const string KEY_MODE = "mode";
    private const string KEY_USE_QUESTION_MARKS = "use-question-marks";
    private const string KEY_USE_OVERMINE_WARNING = "use-overmine-warning";
    private const string KEY_USE_AUTOFLAG = "use-autoflag";

    /* Faces for new game button */
    private Gtk.ToolButton face_button;
    private Gtk.Image win_face_image;
    private Gtk.Image sad_face_image;
    private Gtk.Image smile_face_image;
    private Gtk.Image cool_face_image;
    private Gtk.Image worried_face_image;

    /* Main window */
    private Gtk.Window window;

    /* Minefield being played */
    private Minefield minefield;

    /* Minefield widget */
    private MinefieldView minefield_view;

    private Gtk.Dialog? pref_dialog = null;
    private Gtk.Label flag_label;
    private Gtk.SpinButton n_mines_spin;
    private GnomeGamesSupport.Clock clock;
    private SimpleAction pause;
    private SimpleAction hint;
    private Gtk.Action hint_action;
    private GnomeGamesSupport.FullscreenAction fullscreen_action;
    private GnomeGamesSupport.PauseAction pause_action;
    private Gtk.AspectFrame new_game_screen;
    private Gtk.AspectFrame custom_game_screen;
    private bool is_new_game_screen;

    private const GLib.ActionEntry[] action_entries =
    {
        { "new-game",      new_game_cb                                            },
        { "hint",          hint_cb                                                },
        { "pause",         toggle_pause_cb                                        },
        { "fullscreen",    fullscreen_cb                                          },
        { "scores",        scores_cb                                              },
        { "preferences",   preferences_cb                                         },
        { "quit",          quit_cb                                                },
        { "help",          help_cb                                                },
        { "about",         about_cb                                               }
    };

    private const GnomeGamesSupport.ScoresCategory scorecats[] =
    {
        {"Small",  NC_("board size", "Small") },
        {"Medium", NC_("board size", "Medium") },
        {"Large",  NC_("board size", "Large") },
        {"Custom", NC_("board size", "Custom") }
    };

    private GnomeGamesSupport.Scores highscores;

    public GnoMine ()
    {
        Object (application_id: "org.gnome.gnomine", flags: ApplicationFlags.FLAGS_NONE);
    }

    protected override void startup ()
    {
        base.startup ();

        Environment.set_application_name (_("Mines"));

        settings = new Settings ("org.gnome.gnomine");

        highscores = new GnomeGamesSupport.Scores ("gnomine", scorecats, "board size", null, 0 /* default category */, GnomeGamesSupport.ScoreStyle.TIME_ASCENDING);

        Gtk.Window.set_default_icon_name ("gnome-mines");

        add_action_entries (action_entries, this);
        hint = lookup_action ("hint") as SimpleAction;
        hint.set_enabled (false);
        pause = lookup_action ("pause") as SimpleAction;
        pause.set_enabled (false);

        var builder = new Gtk.Builder ();
        try
        {
            builder.add_from_file (Path.build_filename (DATA_DIRECTORY, "gnomine.ui"));
        }
        catch (Error e)
        {
            error ("Unable to build menus: %s", e.message);
        }
        set_app_menu (builder.get_object ("gnomine-menu") as MenuModel);

        window = new Gtk.ApplicationWindow (this);
        window.title = _("Mines");

        GnomeGamesSupport.settings_bind_window_state ("/org/gnome/gnomine/", window);
        add_window (window);

        GnomeGamesSupport.stock_init ();

        var main_vbox = new Gtk.Box (Gtk.Orientation.VERTICAL, 0);
        window.add (main_vbox);
        main_vbox.show ();

        var action_group = new Gtk.ActionGroup ("group");
        action_group.set_translation_domain (GETTEXT_PACKAGE);
        action_group.add_actions (actions, this);

        var ui_manager = new Gtk.UIManager ();
        ui_manager.insert_action_group (action_group, 0);
        try
        {
            ui_manager.add_ui_from_string (ui_description, -1);
        }
        catch (Error e)
        {
        }
        hint_action = action_group.get_action ("Hint");
        hint_action.is_important = true;
        hint_action.set_sensitive (false);

        action_group.get_action ("NewGame").is_important = true;

        fullscreen_action = new GnomeGamesSupport.FullscreenAction ("Fullscreen", window);
        action_group.add_action_with_accel (fullscreen_action, null);

        pause_action = new GnomeGamesSupport.PauseAction ("PauseGame");
        pause_action.set_sensitive (false);
        pause_action.state_changed.connect (pause_cb);
        action_group.add_action_with_accel (pause_action, null);

        window.add_accel_group (ui_manager.get_accel_group ());

        var status_box = new Gtk.Box (Gtk.Orientation.HORIZONTAL, 10);
        status_box.show ();

        /* show the numbers of total and remaining mines */
        flag_label = new Gtk.Label ("");
        flag_label.show ();

        status_box.pack_start (flag_label, false, false, 0);

        /* game clock */
        var box = new Gtk.Box (Gtk.Orientation.HORIZONTAL, 0);
        box.show ();
        status_box.pack_start (box, false, false, 0);

        var label = new Gtk.Label (_("Time: "));
        box.pack_start (label, false, false, 0);
        label.show ();

        clock = new GnomeGamesSupport.Clock ();
        clock.show ();
        box.pack_start (clock, false, false, 0);

        var status_alignment = new Gtk.Alignment (1.0f, 0.5f, 0.0f, 0.0f);
        status_alignment.add (status_box);
        status_alignment.show();

        var status_item = new Gtk.ToolItem ();
        status_item.set_expand (true);
        status_item.add (status_alignment);
        status_item.show();

        /* create fancy faces */
        win_face_image = load_face_image ("face-win.svg");
        sad_face_image = load_face_image ("face-sad.svg");
        smile_face_image = load_face_image ("face-smile.svg");
        cool_face_image = load_face_image ("face-cool.svg");
        worried_face_image = load_face_image ("face-worried.svg");

        /* initialize toolbar */
        var toolbar = (Gtk.Toolbar) ui_manager.get_widget ("/Toolbar");
        toolbar.show_arrow = false;
        face_button = (Gtk.ToolButton) ui_manager.get_widget ("/Toolbar/NewGame");
        toolbar.get_style_context ().add_class (Gtk.STYLE_CLASS_PRIMARY_TOOLBAR);
        /* replace the dull new-game icon with fancy faces */
        set_face_image (smile_face_image);

        toolbar.insert (status_item, -1);
        main_vbox.pack_start (toolbar, false, false, 0);

        var view_box = new Gtk.Box (Gtk.Orientation.VERTICAL, 0);
        view_box.border_width = 3;
        view_box.show ();
        main_vbox.pack_start (view_box, true, true, 0);

        minefield_view = new MinefieldView ();
        minefield_view.set_use_question_marks (settings.get_boolean (KEY_USE_QUESTION_MARKS));
        minefield_view.set_use_overmine_warning (settings.get_boolean (KEY_USE_OVERMINE_WARNING));
        minefield_view.set_use_autoflag (settings.get_boolean (KEY_USE_AUTOFLAG));
        minefield_view.button_press_event.connect (view_button_press_event);
        minefield_view.look.connect (look_cb);
        minefield_view.unlook.connect (unlook_cb);
        view_box.pack_start (minefield_view, true, true, 0);

        /* New game screen */
        new_game_screen = new Gtk.AspectFrame (_("Field Size"), 0.5f, 0.5f, 1.0f, false);
        new_game_screen.set_shadow_type (Gtk.ShadowType.NONE);
        new_game_screen.set_size_request(200, 200);

        var new_game_table = new Gtk.Table (2, 2, true);
        new_game_screen.add (new_game_table);

        var button_small = new Gtk.Button ();
        new_game_table.attach_defaults (button_small, 0, 1, 0, 1);
        button_small.clicked.connect (small_size_clicked_cb);

        label = new Gtk.Label (null);
        label.set_markup (make_minefield_description ("#0000ff", 8, 8, 10));
        label.set_justify (Gtk.Justification.CENTER);
        button_small.add (label);

        var button_medium = new Gtk.Button ();
        new_game_table.attach_defaults (button_medium, 1, 2, 0, 1);
        button_medium.clicked.connect (medium_size_clicked_cb);

        label = new Gtk.Label (null);
        label.set_markup (make_minefield_description ("#00a000", 16, 16, 40));
        label.set_justify (Gtk.Justification.CENTER);
        button_medium.add (label);

        var button_large = new Gtk.Button ();
        new_game_table.attach_defaults (button_large, 0, 1, 1, 2);
        button_large.clicked.connect (large_size_clicked_cb);

        label = new Gtk.Label (null);
        label.set_markup (make_minefield_description ("#ff0000", 30, 16, 99));
        label.set_justify (Gtk.Justification.CENTER);
        button_large.add (label);

        var button_custom = new Gtk.Button ();
        new_game_table.attach_defaults (button_custom, 1, 2, 1, 2);
        button_custom.clicked.connect (show_custom_game_screen);

        label = new Gtk.Label (null);
        label.set_markup_with_mnemonic ("<span fgcolor='#00007f'><span size='xx-large' weight='heavy'>?</span>\n" + dpgettext2 (null, "board size", "_Custom") + "</span>");
        label.set_justify (Gtk.Justification.CENTER);
        button_custom.add (label);

        new_game_screen.show_all ();
        view_box.pack_start (new_game_screen, true, true, 0);

        /* Custom game screen */
        custom_game_screen = new Gtk.AspectFrame ("", 0.5f, 0.5f, 0.0f, true);
        custom_game_screen.set_shadow_type (Gtk.ShadowType.NONE);

        var custom_game_frame = new GnomeGamesSupport.Frame (_("Custom Size"));
        custom_game_screen.add (custom_game_frame);

        var custom_field_grid = new Gtk.Grid ();
        custom_field_grid.set_row_spacing (6);
        custom_field_grid.set_column_spacing (12);
        custom_game_frame.add (custom_field_grid);

        label = new Gtk.Label.with_mnemonic (_("H_orizontal:"));
        label.set_alignment (0, 0.5f);
        custom_field_grid.attach (label, 0, 0, 1, 1);

        var field_width_entry = new Gtk.SpinButton.with_range (XSIZE_MIN, XSIZE_MAX, 1);
        field_width_entry.value_changed.connect (xsize_spin_cb);
        field_width_entry.set_value (settings.get_int (KEY_XSIZE));
        custom_field_grid.attach (field_width_entry, 1, 0, 1, 1);
        label.set_mnemonic_widget (field_width_entry);

        label = new Gtk.Label.with_mnemonic (_("_Vertical:"));
        label.set_alignment (0, 0.5f);
        custom_field_grid.attach (label, 0, 1, 1, 1);

        var field_height_entry = new Gtk.SpinButton.with_range (YSIZE_MIN, YSIZE_MAX, 1);
        field_height_entry.value_changed.connect (ysize_spin_cb);
        field_height_entry.set_value (settings.get_int (KEY_YSIZE));
        custom_field_grid.attach (field_height_entry, 1, 1, 1, 1);
        label.set_mnemonic_widget (field_height_entry);

        label = new Gtk.Label.with_mnemonic (_("_Number of mines:"));
        label.set_alignment (0, 0.5f);
        custom_field_grid.attach (label, 0, 2, 1, 1);

        n_mines_spin = new Gtk.SpinButton.with_range (1, XSIZE_MAX * YSIZE_MAX, 1);
        n_mines_spin.value_changed.connect (n_mines_spin_cb);
        n_mines_spin.set_value (settings.get_int (KEY_NMINES));
        custom_field_grid.attach (n_mines_spin, 1, 2, 1, 1);

        set_n_mines_limit ();
        label.set_mnemonic_widget (n_mines_spin);

        var hbox = new Gtk.HBox (false, 5);
        custom_field_grid.attach (hbox, 0, 3, 2, 1);

        var button_back = new Gtk.Button.from_stock (Gtk.Stock.CANCEL);
        button_back.clicked.connect (show_new_game_screen);
        hbox.pack_start (button_back, true, true);

        button_custom = new Gtk.Button.with_mnemonic ("_Play Game");
        button_custom.set_image (new Gtk.Image.from_stock (Gtk.Stock.GO_FORWARD, Gtk.IconSize.BUTTON));
        button_custom.clicked.connect (custom_size_clicked_cb);
        hbox.pack_start (button_custom, true, true);

        custom_game_screen.show_all ();
        custom_game_screen.hide ();
        view_box.pack_start (custom_game_screen, true, false);
    }
    
    private string make_minefield_description (string color, int width, int height, int n_mines)
    {
        var size_label = "%d × %d".printf (width, height);
        var mines_label = ngettext ("<b>%d</d> mine", "<b>%d</b> mines", n_mines).printf (n_mines);
        return "<span fgcolor='%s'><span size='x-large' weight='ultrabold'>%s</span>\n%s</span>".printf (color, size_label, mines_label);
    }

    private const Gtk.ActionEntry actions[] =
    {
        {"NewGame", GnomeGamesSupport.STOCK_NEW_GAME, null, null, N_("Start a new game"), new_game_cb},
        {"Hint", GnomeGamesSupport.STOCK_HINT, null, null, N_("Show a hint"), hint_cb}
    };

    private const string ui_description =
        "<ui>" +
        "    <toolbar name='Toolbar'>" +
        "        <toolitem action='NewGame'/>" +
        "        <toolitem action='Hint'/>" +
        "        <toolitem action='PauseGame'/>" +
        "        <toolitem action='Fullscreen'/>" +
        "    </toolbar>" +
        "</ui>";

    public void start ()
    {
        window.show ();
        show_new_game_screen ();
        set_face_image (smile_face_image);
    }

    public override void activate ()
    {
        window.show ();
    }

    private Gtk.Image load_face_image (string name)
    {
        var image = new Gtk.Image ();
        var filename = Path.build_filename (DATA_DIRECTORY, name);

        if (filename != null)
            image.set_from_file (filename);

        image.show ();

        return image;
    }

    private void set_face_image (Gtk.Image face_image)
    {
        face_button.set_icon_widget (face_image);
    }

    private bool view_button_press_event (Gtk.Widget widget, Gdk.EventButton event)
    {
        /* Cancel pause on click */
        if (pause_action.get_is_paused ())
        {
            pause_action.set_is_paused (false);
            return true;
        }

        return false;
    }

    private void quit_cb ()
    {
        window.destroy ();
    }

    private void update_flag_label ()
    {
        flag_label.set_text ("Flags: %u/%u".printf (minefield.n_flags, minefield.n_mines));
    }

    /* Show the high scores dialog - creating it if necessary. If pos is
     * greater than 0 the appropriate score is highlighted. If the score isn't
     * a high score and this isn't a direct request to see the scores, we
     * only show a simple dialog. */
    private int show_scores (int pos, bool endofgame)
    {
        if (endofgame && (pos <= 0))
        {
            var dialog = new Gtk.MessageDialog.with_markup (window,
                                                            Gtk.DialogFlags.DESTROY_WITH_PARENT,
                                                            Gtk.MessageType.INFO,
                                                            Gtk.ButtonsType.NONE,
                                                            "<b>%s</b>\n%s",
                                                            _("The Mines Have Been Cleared!"),
                                                            _("Great work, but unfortunately your score did not make the top ten."));
            dialog.add_buttons (Gtk.Stock.QUIT, Gtk.ResponseType.REJECT,
                                _("_New Game"), Gtk.ResponseType.ACCEPT, null);
            dialog.set_default_response (Gtk.ResponseType.ACCEPT);
            dialog.set_title ("");
            var result = dialog.run ();
            dialog.destroy ();
            return result;
        }
        else
        {
            var dialog = new GnomeGamesSupport.ScoresDialog (window, highscores, _("Mines Scores"));
            dialog.set_category_description (_("Size:"));

            if (pos > 0)
            {
                dialog.set_hilight (pos);
                var message = "<b>%s</b>\n\n%s".printf (_("Congratulations!"), pos == 1 ? _("Your score is the best!") : _("Your score has made the top ten."));
                dialog.set_message (message);
            }
            else
                dialog.set_message (null);

            if (endofgame)
                dialog.set_buttons (GnomeGamesSupport.ScoresButtons.QUIT_BUTTON | GnomeGamesSupport.ScoresButtons.NEW_GAME_BUTTON);
            else
                dialog.set_buttons (0);
            var result = dialog.run ();
            dialog.destroy ();
            return result;
        }
    }

    private void fullscreen_cb ()
    {
        fullscreen_action.set_is_fullscreen (!fullscreen_action.get_is_fullscreen ());
    }

    private void scores_cb ()
    {
        show_scores (0, false);
    }

    private void show_custom_game_screen ()
    {
        is_new_game_screen = false;
        custom_game_screen.show ();
        minefield_view.hide ();
        new_game_screen.hide ();
    }

    private void show_new_game_screen ()
    {
        if (is_new_game_screen)
            return;

        if (minefield != null && minefield.n_cleared > 0 && !minefield.exploded && !minefield.is_complete)
        {
            var dialog = new Gtk.MessageDialog (window, Gtk.DialogFlags.MODAL, Gtk.MessageType.QUESTION, Gtk.ButtonsType.NONE, "%s", _("Cancel current game?"));
            dialog.add_buttons (_("Start New Game"), Gtk.ResponseType.ACCEPT,
                                _("Keep Current Game"), Gtk.ResponseType.REJECT,
                                null);
            var result = dialog.run ();
            dialog.destroy ();
            if (result == Gtk.ResponseType.REJECT)
                return;
        }

        minefield = null;

        is_new_game_screen = true;
        custom_game_screen.hide ();
        minefield_view.hide ();
        new_game_screen.show ();
        flag_label.set_text("");
        clock.stop ();
        clock.reset ();
        set_face_image (smile_face_image);

        hint.set_enabled (false);
        hint_action.set_sensitive (false);
        pause.set_enabled (false);
        pause_action.set_sensitive (false);

        minefield_view.paused = false;
        pause_action.set_is_paused (false);
    }

    private void new_game ()
    {
        is_new_game_screen = false;
        custom_game_screen.hide ();
        minefield_view.show ();
        new_game_screen.hide ();

        clock.reset ();
        set_face_image (smile_face_image);

        int x, y, n;
        var score_key = "";
        switch (settings.get_int (KEY_MODE))
        {
        case 0:
            x = 8;
            y = 8;
            n = 10;
            score_key = "Small";
            break;
        case 1:
            x = 16;
            y = 16;
            n = 40;
            score_key = "Medium";
            break;
        case 2:
            x = 30;
            y = 16;
            n = 99;
            score_key = "Large";
            break;
        default:
        case 3:
            x = settings.get_int (KEY_XSIZE).clamp (XSIZE_MIN, XSIZE_MAX);
            y = settings.get_int (KEY_YSIZE).clamp (YSIZE_MIN, YSIZE_MAX);
            n = settings.get_int (KEY_NMINES).clamp (1, x * y - 10);
            score_key = "Custom";
            break;
        }

        highscores.set_category (score_key);
        if (minefield != null)
            SignalHandler.disconnect_by_func (minefield, null, this);
        minefield = new Minefield (x, y, n);
        minefield.marks_changed.connect (marks_changed_cb);
        minefield.explode.connect (explode_cb);
        minefield.cleared.connect (cleared_cb);

        minefield_view.minefield = minefield;

        update_flag_label ();

        hint.set_enabled (true);
        hint_action.set_sensitive (true);
        pause.set_enabled (true);
        pause_action.set_sensitive (true);

        minefield_view.paused = false;
        pause_action.set_is_paused (false);
    }

    private void hint_cb ()
    {
        uint x, y;
        minefield.hint (out x, out y);

        /* There is a ten second penalty for accepting a hint. */
        minefield.clear_mine (x, y);
        clock.add_seconds (10);
    }

    private void new_game_cb ()
    {
        if (is_new_game_screen)
            new_game ();
        else
            show_new_game_screen ();
    }

    private void toggle_pause_cb ()
    {
        pause_action.set_is_paused (!pause_action.get_is_paused ());
    }

    private void pause_cb ()
    {
        if (pause_action.get_is_paused ())
        {
            minefield_view.paused = true;
            hint.set_enabled (false);
            hint_action.set_sensitive (false);
            clock.stop ();
        }
        else
        {
            minefield_view.paused = false;
            hint.set_enabled (true);
            hint_action.set_sensitive (true);
            clock.start ();
        }
    }

    private void marks_changed_cb (Minefield minefield)
    {
        update_flag_label ();
        clock.start ();
    }

    private void explode_cb (Minefield minefield)
    {
        set_face_image (sad_face_image);
        hint.set_enabled (false);
        hint_action.set_sensitive (false);
        pause.set_enabled (false);
        pause_action.set_sensitive (false);
        clock.stop ();
    }

    private void cleared_cb (Minefield minefield)
    {
        clock.stop ();

        set_face_image (win_face_image);

        var seconds = clock.get_seconds ();
        var pos = highscores.add_time_score ((float) (seconds / 60) + (float) (seconds % 60) / 100);

        if (show_scores (pos, true) == Gtk.ResponseType.REJECT)
            window.destroy ();
        else
            show_new_game_screen ();
    }

    private void look_cb (MinefieldView minefield_view)
    {
        set_face_image (worried_face_image);
        clock.start ();
    }

    private void unlook_cb (MinefieldView minefield_view)
    {
        set_face_image (cool_face_image);
    }

    private void about_cb ()
    {
        string[] authors =
        {
            _("Main game:"),
            "Pista",
            "Szekeres Istvan",
            "Robert Ancell",
            "",
            _("Score:"),
            "Horacio J. Pe\xc3\xb1a",
            "",
            _("Resizing and SVG support:"),
            "Steve Chaplin",
            "Callum McKenzie",
            null
        };

        string[] artists =
        {
            _("Faces:"),
            "tigert",
            "Lapo Calamandrei and Ulisse Perusin",
            "",
            _("Graphics:"),
            "Richard Hoelscher",
            null
        };

        string[] documenters =
        {
            "Callum McKenzie",
            null
        };

        Gtk.show_about_dialog (window,
                               "name", _("Mines"),
                               "version", VERSION,
                               "comments",
                               _("The popular logic puzzle minesweeper. Clear mines from a board using hints from squares you have already uncovered.\n\nMines is a part of GNOME Games."),
                               "copyright",
                               "Copyright \xc2\xa9 1997-2008 Free Software Foundation, Inc.",
                               "license", GnomeGamesSupport.get_license (_("Mines")),
                               "authors", authors,
                               "artists", artists,
                               "documenters", documenters,
                               "translator-credits", _("translator-credits"),
                               "logo-icon-name", "gnomine", "website",
                               "http://www.gnome.org/projects/gnome-games/",
                               "website-label", _("GNOME Games web site"),
                               "wrap-license", true, null);
    }

    private void set_n_mines_limit ()
    {
        /* Fix up the maximum number of mines so that there is always at least
         * ten free spaces. Nine are so we can clear at least the immediate
         * eight neighbours at the start and one more so the game isn't over
         * immediately. */
        var max_mines = settings.get_int (KEY_XSIZE) * settings.get_int (KEY_YSIZE) - 10;
        if (settings.get_int (KEY_NMINES) > max_mines)
        {
            settings.set_int (KEY_NMINES, max_mines);
            n_mines_spin.set_value (max_mines);
        }
        n_mines_spin.set_range (1, max_mines);
    }

    private void xsize_spin_cb (Gtk.SpinButton spin)
    {
        var xsize = spin.get_value_as_int ();        
        if (xsize == settings.get_int (KEY_XSIZE))
            return;

        settings.set_int (KEY_XSIZE, xsize);
        set_n_mines_limit ();
    }

    private void ysize_spin_cb (Gtk.SpinButton spin)
    {
        var ysize = spin.get_value_as_int ();        
        if (ysize == settings.get_int (KEY_YSIZE))
            return;

        settings.set_int (KEY_YSIZE, ysize);
        set_n_mines_limit ();
    }

    private void n_mines_spin_cb (Gtk.SpinButton spin)
    {
        var n_mines = spin.get_value_as_int ();
        if (n_mines == settings.get_int (KEY_NMINES))
            return;

        settings.set_int (KEY_NMINES, n_mines);
    }

    private void use_question_toggle_cb (Gtk.ToggleButton button)
    {
        var use_question_marks = button.get_active ();
        settings.set_boolean (KEY_USE_QUESTION_MARKS, use_question_marks);
        minefield_view.set_use_question_marks (use_question_marks);
    }

    private void use_overmine_toggle_cb (Gtk.ToggleButton button)
    {
        var use_overmine_warning = button.get_active ();
        settings.set_boolean (KEY_USE_OVERMINE_WARNING, use_overmine_warning);
        minefield_view.set_use_overmine_warning (use_overmine_warning);
    }

    private Gtk.Dialog create_preferences ()
    {
        var vbox = new Gtk.VBox (false, 5);

        var frame = new GnomeGamesSupport.Frame (_("Flags"));
        vbox.pack_start (frame, false, false);
        
        var flag_options_vbox = new Gtk.Box (Gtk.Orientation.VERTICAL, 6);
        flag_options_vbox.show ();
        frame.add (flag_options_vbox);

        var question_toggle = new Gtk.CheckButton.with_mnemonic (_("_Use \"I'm not sure\" flags"));
        question_toggle.toggled.connect (use_question_toggle_cb);
        question_toggle.set_active (settings.get_boolean (KEY_USE_QUESTION_MARKS));
        flag_options_vbox.pack_start (question_toggle, false, true, 0);

        var overmine_toggle = new Gtk.CheckButton.with_mnemonic (_("_Warn if too many flags placed"));
        overmine_toggle.toggled.connect (use_overmine_toggle_cb);
        overmine_toggle.set_active (settings.get_boolean (KEY_USE_OVERMINE_WARNING));
        flag_options_vbox.pack_start (overmine_toggle, false, true, 0);

        var dialog = new Gtk.Dialog.with_buttons (_("Mines Preferences"),
                                                  window,
                                                  0,
                                                  Gtk.Stock.CLOSE,
                                                  Gtk.ResponseType.CLOSE, null);
        dialog.set_border_width (5);
        dialog.set_resizable (false);
        var box = (Gtk.Box) dialog.get_content_area ();
        box.set_spacing (2);
        box.pack_start (vbox, false, false, 0);

        dialog.response.connect (pref_response_cb);
        dialog.delete_event.connect (pref_delete_event_cb);

        vbox.show_all ();

        return dialog;
    }
    
    private void set_mode (int mode)
    {
        if (mode != settings.get_int (KEY_MODE))
            settings.set_int (KEY_MODE, mode);

        new_game ();
    }

    private void small_size_clicked_cb ()
    {
        set_mode (0);
    }

    private void medium_size_clicked_cb ()
    {
        set_mode (1);
    }

    private void large_size_clicked_cb ()
    {
        set_mode (2);
    }

    private void custom_size_clicked_cb ()
    {
        set_mode (3);
    }
    
    private void pref_response_cb (Gtk.Dialog dialog, int response_id)
    {
        pref_dialog.hide ();
    }

    private bool pref_delete_event_cb (Gtk.Widget widget, Gdk.EventAny event)
    {
        pref_dialog.hide ();
        return true;
    }

    private void preferences_cb ()
    {
        if (pref_dialog == null)
            pref_dialog = create_preferences ();
        pref_dialog.present ();
    }

    private void help_cb ()
    {
        try
        {
            Gtk.show_uri (window.get_screen (), "help:gnomine", Gtk.get_current_event_time ());
        }
        catch (Error e)
        {
            warning ("Failed to show help: %s", e.message);
        }
    }


    public static int main (string[] args)
    {
        Intl.setlocale (LocaleCategory.ALL, "");
        Intl.bindtextdomain (GETTEXT_PACKAGE, LOCALEDIR);
        Intl.bind_textdomain_codeset (GETTEXT_PACKAGE, "UTF-8");
        Intl.textdomain (GETTEXT_PACKAGE);

        GnomeGamesSupport.scores_startup ();

        var context = new OptionContext ("");
        context.set_translation_domain (GETTEXT_PACKAGE);
        context.add_group (Gtk.get_option_group (true));

        try
        {
            context.parse (ref args);
        }
        catch (Error e)
        {
            stderr.printf ("%s\n", e.message);
            return Posix.EXIT_FAILURE;
        }

        var app = new GnoMine ();
        return app.run ();
    }
}
