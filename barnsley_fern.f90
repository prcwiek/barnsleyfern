! Barnsley fern in GTK3
! https://en.wikipedia.org/wiki/Barnsley_fern
! https://pl.wikipedia.org/wiki/Paproc_Barnsleya

module global_widgets

    use, intrinsic :: iso_c_binding, only: c_ptr, c_char, c_int
    
    !integer(c_int) :: nch, rowstride, pixwidth, pixheight     
    integer(c_int) :: nch, rowstride, pixwidth, pixheight 
    character(c_char), dimension(:), pointer :: pixel
    
    type(c_ptr) :: draw_area
    type(c_ptr) :: pixbuf_area
    type(c_ptr) :: spin_btn_iter, spin_btn_scale
    type(c_ptr) :: radio_1, radio_2
    
    
end module global_widgets

module handlers
    
    use gtk, only: gtk_init, gtk_window_new, gtk_box_new, gtk_window_set_title,&
                   gtk_window_set_default_size, gtk_widget_show, gtk_main,&
                   gtk_container_add, gtk_button_new_with_label,&
                   gtk_widget_set_margin_start, gtk_widget_set_margin_end,&
                   gtk_widget_set_margin_top, gtk_widget_set_margin_bottom,&
                   gtk_adjustment_new, gtk_grid_new, gtk_expander_new, gtk_grid_attach,&
                   gtk_grid_set_column_homogeneous, gtk_grid_set_row_homogeneous,&
                   gtk_expander_set_expanded, gtk_widget_show_all, gtk_label_new,&
                   gtk_spin_button_new, gtk_drawing_area_new, gtk_main_quit,&
                   gtk_widget_set_vexpand, gtk_widget_queue_draw, gtk_notebook_new,&
                   gtk_notebook_append_page, gtk_label_new_with_mnemonic,&
                   gtk_spin_button_get_value, gtk_window_set_resizable,&
                   gtk_radio_button_new_with_label, gtk_radio_button_get_group,&
                   gtk_toggle_button_set_active, gtk_toggle_button_get_active,&
                   gtk_spin_button_set_value,&
                   g_signal_connect,&
                   GTK_WINDOW_TOPLEVEL,&
                   GTK_ORIENTATION_HORIZONTAL, GTK_ORIENTATION_VERTICAL,&
                   GDK_COLORSPACE_RGB, c_null_char, c_null_ptr, TRUE, FALSE
    
    use gdk_pixbuf, only: gdk_pixbuf_get_n_channels, gdk_pixbuf_get_pixels, &
                          gdk_pixbuf_get_rowstride, gdk_pixbuf_new
    
    use gdk, only : gdk_cairo_set_source_pixbuf
    
    use gtk_os_dependent, only: gdk_pixbuf_savev
    
    use cairo, only: cairo_paint, cairo_set_source, cairo_surface_write_to_png,&
                     cairo_surface_destroy
                     
    implicit none

contains

    function delete_event(widget, event, gdata) result(res) bind(c)
        use, intrinsic :: iso_c_binding, only: c_ptr, c_int

        integer (c_int) :: res
        type(c_ptr), value :: widget, event, gdata

        res =FALSE
        call gtk_main_quit()
    end function delete_event
    
    function draw(widget, my_cairo_context, gdata) result(res) bind(c)
        use, intrinsic :: iso_c_binding, only: c_ptr, c_int
        use global_widgets

        implicit none
        
        integer(c_int)  :: res
        type(c_ptr), value, intent(in) :: widget, my_cairo_context, gdata
        
        call gdk_cairo_set_source_pixbuf(my_cairo_context, pixbuf_area, 0d0, 0d0)
        call cairo_paint(my_cairo_context)
        
        res = FALSE
    end function draw

    recursive function start_calculations(widget, gdata) result(res) bind(c)
        use, intrinsic :: iso_c_binding, only: c_ptr, c_int
        use global_widgets
        
        implicit none
        
        integer :: niter
        real :: sc
        integer(c_int) :: res
        type(c_ptr), value :: widget, gdata
        
        ! get number of iterations
        niter = INT(gtk_spin_button_get_value(spin_btn_iter))
        sc = INT(gtk_spin_button_get_value(spin_btn_scale))
        
        call fern(niter, sc)
        
        call gtk_widget_queue_draw(draw_area)
        
        res = FALSE
    end function start_calculations
    
    function clean_draw_area(widget, gdata) result(res) bind(c)
        use, intrinsic :: iso_c_binding, only: c_ptr, c_int
        use global_widgets
        
        implicit none

        type(c_ptr), value :: widget, gdata
        integer(c_int) :: res
        
        pixel = char(0)
        
        call gtk_widget_queue_draw(draw_area)
        
        res = FALSE
    end function clean_draw_area
        
    function save_draw_area(widget, gdata) result(res) bind(c)
        use, intrinsic :: iso_c_binding, only : c_ptr, c_int
        use global_widgets
        
        implicit none
        
        type(c_ptr), value :: widget, gdata
        integer(c_int) :: res
        
        integer(c_int) :: cstatus
        
        cstatus = gdk_pixbuf_savev(pixbuf_area, "fern.png"//c_null_char, "png"//c_null_char,&
                  c_null_ptr, c_null_ptr, c_null_ptr)
        
        res = FALSE
    end function save_draw_area
    
    function radio_1_selected(widget, gdata) result(res) bind(c)
        use, intrinsic :: iso_c_binding, only : c_ptr
        use global_widgets
        
        implicit none
        
        type(c_ptr), value :: widget, gdata
        integer(c_int) :: res
        
        call gtk_spin_button_set_value(spin_btn_scale, 50d0)

        res = FALSE
    end function radio_1_selected        
        
    function radio_2_selected(widget, gdata) result(res) bind(c)
        use, intrinsic :: iso_c_binding, only : c_ptr
        use global_widgets
        
        implicit none
        
        type(c_ptr), value :: widget, gdata
        integer(c_int) :: res
        
        call gtk_spin_button_set_value(spin_btn_scale, 60d0)

        res = FALSE
    end function radio_2_selected        
        
        
end module handlers

!*************************************************
! Main program
!*************************************************
program barnsley_fern_gtk

    use, intrinsic ::   iso_c_binding, only: c_ptr, c_char, c_funloc, c_f_pointer,&
                        c_int, dp => c_double

    use handlers
    
    use global_widgets
    
    implicit none
    
    type(c_ptr) :: window
    type(c_ptr) :: table
    type(c_ptr) :: box_main, box_btn, box_draw
    type(c_ptr) :: btn_start, btn_save, btn_clean, btn_quit
    type(c_ptr) :: label_iter, label_scale, label_radio
    type(c_ptr) :: radio_group

    call gtk_init()
    
    ! main window
    window = gtk_window_new(GTK_WINDOW_TOPLEVEL)
    call gtk_window_set_default_size(window, 720, 720)
    call gtk_window_set_title(window, "Barnsley fern in GTK-3"//c_null_char)
    call gtk_window_set_resizable(window, FALSE)
    call g_signal_connect(window, "delete-event"//c_null_char, c_funloc(delete_event))

    ! main buttons
    btn_start = gtk_button_new_with_label("Start"//c_null_char)
    call g_signal_connect(btn_start, "clicked"//c_null_char, c_funloc(start_calculations))

    btn_clean = gtk_button_new_with_label("Clean"//c_null_char)
    call g_signal_connect(btn_clean, "clicked"//c_null_char, c_funloc(clean_draw_area))
    
    btn_save = gtk_button_new_with_label("Save"//c_null_char)
    call g_signal_connect(btn_save, "clicked"//c_null_char, c_funloc(save_draw_area))
    
    btn_quit = gtk_button_new_with_label("Quit"//c_null_char)
    !print*, "before"
    call g_signal_connect(btn_quit, "clicked"//c_null_char, c_funloc(delete_event))
    
    ! spin button to set a nmber of iterations
    label_iter = gtk_label_new("Number of iterations:"//c_null_char)
    spin_btn_iter = gtk_spin_button_new(gtk_adjustment_new(10000d0, 5000d0, 20000000d0, 500d0,&
                    500d0,0d0), 0.05d0, 0_c_int)

    ! spin button to set a scale parameter
    label_scale = gtk_label_new("Scale parameter:"//c_null_char)
    spin_btn_scale = gtk_spin_button_new(gtk_adjustment_new(50d0, 5d0, 2000d0, 5d0,&
                    500d0,0d0), 0.05d0, 0_c_int)

    ! creat radio buttons
    radio_group = c_null_ptr
    radio_1 = gtk_radio_button_new_with_label(radio_group, "Barnsley fern"//c_null_char)
  
    call g_signal_connect(radio_1, "clicked"//c_null_char, c_funloc(radio_1_selected))
    radio_group = gtk_radio_button_get_group(radio_1)
    radio_2 = gtk_radio_button_new_with_label(radio_group, "Thelypteridaceae fern"//c_null_char)    
    call g_signal_connect(radio_2, "clicked"//c_null_char, c_funloc(radio_2_selected))
    call gtk_toggle_button_set_active(radio_1, TRUE)
    label_radio = gtk_label_new("Calculations parameters:"//c_null_char)
    
    ! creta grid for buttons and fields
    table = gtk_grid_new()
    call gtk_grid_set_column_homogeneous(table, TRUE)
    call gtk_grid_set_row_homogeneous(table, TRUE)
    ! add buttons
    call gtk_grid_attach(table, label_iter, 0_c_int, 0_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, spin_btn_iter, 1_c_int, 0_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, label_scale, 0_c_int, 1_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, spin_btn_scale, 1_c_int, 1_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, label_radio, 0_c_int, 2_c_int, 1_c_int, 2_c_int)
    call gtk_grid_attach(table, radio_1, 1_c_int, 2_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, radio_2, 1_c_int, 3_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, btn_start, 2_c_int, 0_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, btn_clean, 2_c_int, 1_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, btn_save, 2_c_int, 2_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, btn_quit, 2_c_int, 3_c_int, 1_c_int, 1_c_int)
  
    ! adjust margins of buttons
    call gtk_widget_set_margin_start(spin_btn_iter, 4_c_int)
    call gtk_widget_set_margin_end(spin_btn_iter, 4_c_int)
    call gtk_widget_set_margin_top(spin_btn_iter, 4_c_int)
    call gtk_widget_set_margin_bottom(spin_btn_iter, 4_c_int)
    
    call gtk_widget_set_margin_start(spin_btn_scale, 4_c_int)
    call gtk_widget_set_margin_end(spin_btn_scale, 4_c_int)
    call gtk_widget_set_margin_top(spin_btn_scale, 4_c_int)
    call gtk_widget_set_margin_bottom(spin_btn_scale, 4_c_int)
    
    call gtk_widget_set_margin_start(btn_start, 4_c_int)
    call gtk_widget_set_margin_end(btn_start, 4_c_int)
    call gtk_widget_set_margin_top(btn_start, 4_c_int)
    call gtk_widget_set_margin_bottom(btn_start, 4_c_int)
    
    call gtk_widget_set_margin_start(btn_clean, 4_c_int)
    call gtk_widget_set_margin_end(btn_clean, 4_c_int)
    call gtk_widget_set_margin_top(btn_clean, 4_c_int)
    call gtk_widget_set_margin_bottom(btn_clean, 4_c_int)
    
    call gtk_widget_set_margin_start(btn_save, 4_c_int)
    call gtk_widget_set_margin_end(btn_save, 4_c_int)
    call gtk_widget_set_margin_top(btn_save, 4_c_int)
    call gtk_widget_set_margin_bottom(btn_save, 4_c_int)
    
    call gtk_widget_set_margin_start(btn_quit, 4_c_int)
    call gtk_widget_set_margin_end(btn_quit, 4_c_int)
    call gtk_widget_set_margin_top(btn_quit, 4_c_int)
    call gtk_widget_set_margin_bottom(btn_quit, 4_c_int)
    
    ! adjust margins of labels
    call gtk_widget_set_margin_start(label_iter, 4_c_int)
    call gtk_widget_set_margin_end(label_iter, 4_c_int)
    call gtk_widget_set_margin_top(label_iter, 4_c_int)
    call gtk_widget_set_margin_bottom(label_iter, 4_c_int)
    
    call gtk_widget_set_margin_start(label_scale, 4_c_int)
    call gtk_widget_set_margin_end(label_scale, 4_c_int)
    call gtk_widget_set_margin_top(label_scale, 4_c_int)
    call gtk_widget_set_margin_bottom(label_scale, 4_c_int)
    
    call gtk_widget_set_margin_start(label_radio, 4_c_int)
    call gtk_widget_set_margin_end(label_radio, 4_c_int)
    call gtk_widget_set_margin_top(label_radio, 4_c_int)
    call gtk_widget_set_margin_bottom(label_radio, 4_c_int)
    
    ! adjust margins of radio buttons
    call gtk_widget_set_margin_start(radio_1, 4_c_int)
    call gtk_widget_set_margin_end(radio_1, 4_c_int)
    call gtk_widget_set_margin_top(radio_1, 4_c_int)
    call gtk_widget_set_margin_bottom(radio_1, 4_c_int)
    
    call gtk_widget_set_margin_start(radio_2, 4_c_int)
    call gtk_widget_set_margin_end(radio_2, 4_c_int)
    call gtk_widget_set_margin_top(radio_2, 4_c_int)
    call gtk_widget_set_margin_bottom(radio_2, 4_c_int)

    ! create boxes
    box_main = gtk_box_new(GTK_ORIENTATION_VERTICAL, 4_c_int)
    box_btn = gtk_box_new(GTK_ORIENTATION_VERTICAL, 4_c_int)
    box_draw = gtk_box_new(GTK_ORIENTATION_VERTICAL, 4_c_int)
    
    call gtk_container_add(box_btn, table)
    
    ! Set the border width around the container:
    call gtk_widget_set_margin_start(box_btn, 10_c_int)
    call gtk_widget_set_margin_end(box_btn, 10_c_int)
    call gtk_widget_set_margin_top(box_btn, 10_c_int)
    call gtk_widget_set_margin_bottom(box_btn, 10_c_int)
  
    call gtk_widget_set_margin_start(box_draw, 10_c_int)
    call gtk_widget_set_margin_end(box_draw, 10_c_int)
    call gtk_widget_set_margin_top(box_draw, 10_c_int)
    call gtk_widget_set_margin_bottom(box_draw, 10_c_int)
    
    ! creat draw area
    draw_area = gtk_drawing_area_new()
    call g_signal_connect(draw_area, "draw"//c_null_char, c_funloc(draw))
    call gtk_widget_set_vexpand(draw_area, TRUE)
    call gtk_container_add(box_draw, draw_area)
    
    call gtk_container_add(box_main, box_btn)
    call gtk_container_add(box_main, box_draw)
    call gtk_widget_set_vexpand(box_main, TRUE)
    
    call gtk_container_add(window, box_main)
    
    call gtk_widget_show_all(window)
  
    ! define the size of drawing area
    pixwidth = 700
    pixheight = 520
    pixbuf_area = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8_c_int, pixwidth, pixheight)
    nch = gdk_pixbuf_get_n_channels(pixbuf_area)
    rowstride = gdk_pixbuf_get_rowstride(pixbuf_area)
    call c_f_pointer(gdk_pixbuf_get_pixels(pixbuf_area), pixel, (/pixwidth*pixheight*nch/))
    
    ! make initial backgroud black
    pixel = char(0)

    call gtk_main()
    
end program barnsley_fern_gtk

!*************************************************
! Subroutine with calculations
!*************************************************
subroutine fern(n, sc)
    
    use, intrinsic :: iso_c_binding
    use handlers
    use global_widgets
    
    implicit none
    
    ! number of iterrations
    integer, intent(in) :: n
    ! scale parameter
    real, intent(in) :: sc
    
    ! transformation factors
    double precision, dimension(1:4, 1:6) :: xt, yt
    ! probability factors
    double precision, dimension(1:4) :: pp 
    double precision :: x0, y0, x, y, r
    
    integer :: s                ! transformation number
    integer :: i, p
    
    ! center of coordinate system 
    double precision :: xo, yo

    ! transformatio factors    
    if (gtk_toggle_button_get_active(radio_1) == TRUE) then
        xt(1, [1,2,3]) = [0.00, 0.00, 0.0]
        xt(2, [1,2,3]) = [0.85, 0.04, 0.0]
        xt(3, [1,2,3]) = [0.20, -0.26, 0.0]
        xt(4, [1,2,3]) = [-0.15, 0.28, 0.0]
        
        yt(1, [1,2,3]) = [0.0, 0.16, 0.0]
        yt(2, [1,2,3]) = [-0.04, 0.85, 1.6]
        yt(3, [1,2,3]) = [0.23, 0.22, 1.6]
        yt(4, [1,2,3]) = [0.26, 0.24, 0.44]

        pp = [0.02, 0.86, 0.93, 1.0]
        
        x0 = 2.0
        y0 = 2.0
        
        xo = 0.5 * pixwidth
        yo = 0.975 * pixheight
    else if (gtk_toggle_button_get_active(radio_2) == TRUE) then
        xt(1, [1,2,3]) = [0.00, 0.00, 0.0]
        xt(2, [1,2,3]) = [0.035, -0.2, -0.09]
        xt(3, [1,2,3]) = [0.95, 0.005, -0.002]
        xt(4, [1,2,3]) = [-0.03, 0.2, 0.083]
        
        yt(1, [1,2,3]) = [0.0, 0.25, -0.4]
        yt(2, [1,2,3]) = [0.16, 0.04, 0.02]
        yt(3, [1,2,3]) = [-0.005, 0.93, 0.5]
        yt(4, [1,2,3]) = [0.16, 0.04, 0.12]
        
        pp = [0.02, 0.07, 0.84, 1.0]
        
        x0 = 1.0
        y0 = 1.0
        
        xo = 0.5 * pixwidth
        yo = 0.925 * pixheight
    end if
    
    
    do i = 1, n
        call random_number(r)
        
        ! select transformation
        if (r <= pp(1)) then
            s = 1
        else if (r <= pp(2)) then
            s = 2
        else if (r <= pp(3)) then
            s = 3
        else
            s = 4
        end if
        
        x = xt(s, 1) * x0 + xt(s, 2) * y0 + xt(s, 3)
        y = yt(s, 1) * x0 + yt(s, 2) * y0 + yt(s, 3)
        
        x0 = x
        y0 = y
        
        x = xo + x * sc
        y = yo - y * sc
        
        if (x > 0 .and. x < (pixwidth-1) .and. y > 0 .and. y < (pixheight-1)) then
            ! position of the corresponding pixel in the pixbuffer
            p = 1 + nint(x)*nch + nint(y)*rowstride
            pixel(p) = char(0)
            pixel(p+1) = char(255)
            pixel(p+2) = char(0)
        end if
        
    end do
   
    
end subroutine fern
