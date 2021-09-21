GLADE = '''<?xml version="1.0" encoding="UTF-8"?>
<!-- Generated with glade 3.38.2 -->
<interface>
  <requires lib="gtk+" version="3.22"/>
  <object class="GtkDialog" id="message_window">
    <property name="can-focus">False</property>
    <property name="type-hint">dialog</property>
    <child internal-child="vbox">
      <object class="GtkBox">
        <property name="can-focus">False</property>
        <property name="orientation">vertical</property>
        <property name="spacing">2</property>
        <child internal-child="action_area">
          <object class="GtkButtonBox">
            <property name="can-focus">False</property>
            <property name="layout-style">end</property>
            <child>
              <object class="GtkButton" id="close_message_button">
                <property name="label" translatable="yes">Close</property>
                <property name="visible">True</property>
                <property name="can-focus">True</property>
                <property name="receives-default">True</property>
                <signal name="clicked" handler="on_close_message_button_clicked" swapped="no"/>
              </object>
              <packing>
                <property name="expand">True</property>
                <property name="fill">True</property>
                <property name="position">0</property>
              </packing>
            </child>
            <child>
              <placeholder/>
            </child>
          </object>
          <packing>
            <property name="expand">False</property>
            <property name="fill">False</property>
            <property name="position">0</property>
          </packing>
        </child>
        <child>
          <object class="GtkBox">
            <property name="visible">True</property>
            <property name="can-focus">False</property>
            <property name="orientation">vertical</property>
            <child>
              <object class="GtkLabel" id="message_window_query_label">
                <property name="visible">True</property>
                <property name="can-focus">False</property>
                <property name="label" translatable="yes">QUERY</property>
              </object>
              <packing>
                <property name="expand">False</property>
                <property name="fill">True</property>
                <property name="position">0</property>
              </packing>
            </child>
            <child>
              <object class="GtkLabel" id="message_window_error_label">
                <property name="visible">True</property>
                <property name="can-focus">False</property>
                <property name="halign">center</property>
                <property name="label" translatable="yes">ERROR</property>
              </object>
              <packing>
                <property name="expand">False</property>
                <property name="fill">True</property>
                <property name="position">1</property>
              </packing>
            </child>
          </object>
          <packing>
            <property name="expand">False</property>
            <property name="fill">True</property>
            <property name="position">1</property>
          </packing>
        </child>
      </object>
    </child>
  </object>
  <object class="GtkApplicationWindow" id="rater_window">
    <property name="can-focus">False</property>
    <child>
      <object class="GtkPaned">
        <property name="visible">True</property>
        <property name="can-focus">True</property>
        <child>
          <object class="GtkBox">
            <property name="visible">True</property>
            <property name="can-focus">False</property>
            <property name="margin-start">3</property>
            <property name="margin-end">3</property>
            <property name="margin-top">3</property>
            <property name="margin-bottom">3</property>
            <property name="orientation">vertical</property>
            <child>
              <object class="GtkLabel" id="results_label">
                <property name="visible">True</property>
                <property name="can-focus">False</property>
                <property name="halign">start</property>
                <property name="margin-start">3</property>
                <property name="margin-end">3</property>
                <property name="margin-top">3</property>
                <property name="margin-bottom">3</property>
                <property name="label" translatable="yes"># results</property>
              </object>
              <packing>
                <property name="expand">False</property>
                <property name="fill">True</property>
                <property name="position">0</property>
              </packing>
            </child>
            <child>
              <object class="GtkSearchEntry" id="search_entry">
                <property name="width-request">200</property>
                <property name="visible">True</property>
                <property name="can-focus">True</property>
                <property name="margin-start">3</property>
                <property name="margin-end">3</property>
                <property name="margin-top">3</property>
                <property name="margin-bottom">3</property>
                <property name="primary-icon-name">edit-find-symbolic</property>
                <property name="primary-icon-activatable">False</property>
                <property name="primary-icon-sensitive">False</property>
                <signal name="activate" handler="on_filter_apply_clicked" swapped="no"/>
                <signal name="icon-release" handler="on_filter_apply_clicked" swapped="no"/>
              </object>
              <packing>
                <property name="expand">False</property>
                <property name="fill">True</property>
                <property name="position">1</property>
              </packing>
            </child>
            <child>
              <object class="GtkScrolledWindow">
                <property name="visible">True</property>
                <property name="can-focus">True</property>
                <property name="margin-start">3</property>
                <property name="margin-end">3</property>
                <property name="margin-top">3</property>
                <property name="margin-bottom">3</property>
                <property name="shadow-type">in</property>
                <child>
                  <object class="GtkTreeView" id="search_treeview">
                    <property name="visible">True</property>
                    <property name="can-focus">True</property>
                    <property name="activate-on-single-click">True</property>
                    <signal name="row-activated" handler="on_search_treeview_row_activated" swapped="no"/>
                    <child internal-child="selection">
                      <object class="GtkTreeSelection"/>
                    </child>
                  </object>
                </child>
              </object>
              <packing>
                <property name="expand">True</property>
                <property name="fill">True</property>
                <property name="position">2</property>
              </packing>
            </child>
          </object>
          <packing>
            <property name="resize">False</property>
            <property name="shrink">True</property>
          </packing>
        </child>
        <child>
          <object class="GtkNotebook">
            <property name="visible">True</property>
            <property name="can-focus">True</property>
            <signal name="switch-page" handler="on_switch_page" swapped="no"/>
            <child>
              <!-- n-columns=2 n-rows=10 -->
              <object class="GtkGrid">
                <property name="visible">True</property>
                <property name="can-focus">False</property>
                <property name="margin-start">3</property>
                <property name="margin-end">3</property>
                <property name="margin-top">3</property>
                <property name="margin-bottom">3</property>
                <property name="column-homogeneous">True</property>
                <child>
                  <object class="GtkEntry" id="title_entry">
                    <property name="visible">True</property>
                    <property name="can-focus">True</property>
                    <property name="margin-left">3</property>
                    <property name="margin-right">3</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                    <signal name="changed" handler="on_title_entry_changed" swapped="no"/>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">1</property>
                    <property name="width">2</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkLabel">
                    <property name="visible">True</property>
                    <property name="can-focus">False</property>
                    <property name="tooltip-text" translatable="yes">The title of the work</property>
                    <property name="halign">start</property>
                    <property name="margin-left">3</property>
                    <property name="margin-right">3</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                    <property name="label" translatable="yes">Title</property>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">0</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkLabel">
                    <property name="visible">True</property>
                    <property name="can-focus">False</property>
                    <property name="tooltip-text" translatable="yes">The quality of the work and/or how much one enjoyed it</property>
                    <property name="halign">start</property>
                    <property name="label" translatable="yes">Score</property>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">2</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkComboBoxText" id="score_combobox">
                    <property name="visible">True</property>
                    <property name="can-focus">False</property>
                    <property name="margin-left">3</property>
                    <property name="margin-right">3</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                    <property name="active">10</property>
                    <property name="active-id">0</property>
                    <items>
                      <item id="7" translatable="yes">Extremely High</item>
                      <item id="6" translatable="yes">Moderately High</item>
                      <item id="5" translatable="yes">Slightly High</item>
                      <item id="4" translatable="yes">Neutral</item>
                      <item id="3" translatable="yes">Slightly Low</item>
                      <item id="2" translatable="yes">Moderately Low</item>
                      <item id="1" translatable="yes">Extremely Low</item>
                      <item id="0" translatable="yes">Unspecified</item>
                    </items>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">3</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkLabel" id="id_label">
                    <property name="visible">True</property>
                    <property name="can-focus">False</property>
                    <property name="halign">end</property>
                    <property name="label" translatable="yes">ID</property>
                  </object>
                  <packing>
                    <property name="left-attach">1</property>
                    <property name="top-attach">0</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkLabel">
                    <property name="visible">True</property>
                    <property name="can-focus">False</property>
                    <property name="tooltip-text" translatable="yes">Current progress of experiencing the work</property>
                    <property name="halign">start</property>
                    <property name="margin-left">3</property>
                    <property name="margin-right">3</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                    <property name="label" translatable="yes">Status</property>
                  </object>
                  <packing>
                    <property name="left-attach">1</property>
                    <property name="top-attach">2</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkComboBoxText" id="status_combobox">
                    <property name="visible">True</property>
                    <property name="can-focus">False</property>
                    <property name="margin-left">3</property>
                    <property name="margin-right">3</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                    <property name="active">6</property>
                    <property name="active-id">0</property>
                    <items>
                      <item id="6" translatable="yes">Reviewing</item>
                      <item id="5" translatable="yes">View Again</item>
                      <item id="4" translatable="yes">Completed</item>
                      <item id="3" translatable="yes">In Progress</item>
                      <item id="2" translatable="yes">Dropped</item>
                      <item id="1" translatable="yes">Planning</item>
                      <item id="0" translatable="yes">Unspecified</item>
                    </items>
                  </object>
                  <packing>
                    <property name="left-attach">1</property>
                    <property name="top-attach">3</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkLabel">
                    <property name="visible">True</property>
                    <property name="can-focus">False</property>
                    <property name="tooltip-text" translatable="yes">Any futher notes about the work or rational behind the scoring</property>
                    <property name="halign">start</property>
                    <property name="margin-left">3</property>
                    <property name="margin-right">3</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                    <property name="label" translatable="yes">Comments</property>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">4</property>
                    <property name="width">2</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkScrolledWindow">
                    <property name="width-request">300</property>
                    <property name="height-request">200</property>
                    <property name="visible">True</property>
                    <property name="can-focus">True</property>
                    <property name="shadow-type">in</property>
                    <child>
                      <object class="GtkTextView" id="comments_textview">
                        <property name="visible">True</property>
                        <property name="can-focus">True</property>
                        <property name="margin-left">3</property>
                        <property name="margin-right">3</property>
                        <property name="margin-start">3</property>
                        <property name="margin-end">3</property>
                        <property name="margin-top">3</property>
                        <property name="margin-bottom">3</property>
                        <property name="wrap-mode">word</property>
                      </object>
                    </child>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">5</property>
                    <property name="width">2</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkLabel">
                    <property name="visible">True</property>
                    <property name="can-focus">False</property>
                    <property name="halign">start</property>
                    <property name="label" translatable="yes">Tags</property>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">6</property>
                    <property name="width">2</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkScrolledWindow">
                    <property name="width-request">300</property>
                    <property name="height-request">120</property>
                    <property name="visible">True</property>
                    <property name="can-focus">True</property>
                    <property name="shadow-type">in</property>
                    <child>
                      <object class="GtkTextView" id="tags_textview">
                        <property name="visible">True</property>
                        <property name="can-focus">True</property>
                        <property name="tooltip-text" translatable="yes">Separate tags with commas</property>
                        <property name="margin-start">3</property>
                        <property name="margin-end">3</property>
                        <property name="margin-top">3</property>
                        <property name="margin-bottom">3</property>
                        <property name="wrap-mode">word</property>
                      </object>
                    </child>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">7</property>
                    <property name="width">2</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkButton" id="new_rating_button">
                    <property name="label" translatable="yes">New</property>
                    <property name="visible">True</property>
                    <property name="can-focus">True</property>
                    <property name="receives-default">True</property>
                    <property name="margin-left">3</property>
                    <property name="margin-right">3</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                    <signal name="clicked" handler="on_new_rating_button_clicked" swapped="no"/>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">8</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkButton" id="delete_rating_button">
                    <property name="label" translatable="yes">Delete</property>
                    <property name="visible">True</property>
                    <property name="can-focus">True</property>
                    <property name="receives-default">True</property>
                    <property name="margin-left">3</property>
                    <property name="margin-right">3</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                    <signal name="clicked" handler="on_delete_rating_button_clicked" swapped="no"/>
                  </object>
                  <packing>
                    <property name="left-attach">1</property>
                    <property name="top-attach">8</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkBox">
                    <property name="visible">True</property>
                    <property name="can-focus">False</property>
                    <child>
                      <object class="GtkLabel">
                        <property name="visible">True</property>
                        <property name="can-focus">False</property>
                        <property name="margin-start">3</property>
                        <property name="margin-end">3</property>
                        <property name="margin-top">3</property>
                        <property name="margin-bottom">3</property>
                        <property name="label" translatable="yes">Created: </property>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">0</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="created_label">
                        <property name="visible">True</property>
                        <property name="can-focus">False</property>
                        <property name="margin-start">3</property>
                        <property name="margin-end">3</property>
                        <property name="margin-top">3</property>
                        <property name="margin-bottom">3</property>
                        <property name="label" translatable="yes">####-##-##</property>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">1</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">9</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkBox">
                    <property name="visible">True</property>
                    <property name="can-focus">False</property>
                    <child>
                      <object class="GtkLabel">
                        <property name="visible">True</property>
                        <property name="can-focus">False</property>
                        <property name="margin-start">3</property>
                        <property name="margin-end">3</property>
                        <property name="margin-top">3</property>
                        <property name="margin-bottom">3</property>
                        <property name="label" translatable="yes">Modified: </property>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">0</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkLabel" id="modified_label">
                        <property name="visible">True</property>
                        <property name="can-focus">False</property>
                        <property name="margin-start">3</property>
                        <property name="margin-end">3</property>
                        <property name="margin-top">3</property>
                        <property name="margin-bottom">3</property>
                        <property name="label" translatable="yes">####-##-##</property>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">1</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="left-attach">1</property>
                    <property name="top-attach">9</property>
                  </packing>
                </child>
              </object>
              <packing>
                <property name="tab-expand">True</property>
              </packing>
            </child>
            <child type="tab">
              <object class="GtkLabel">
                <property name="visible">True</property>
                <property name="can-focus">False</property>
                <property name="label" translatable="yes">Ratings</property>
              </object>
              <packing>
                <property name="tab-fill">False</property>
              </packing>
            </child>
            <child>
              <!-- n-columns=2 n-rows=10 -->
              <object class="GtkGrid">
                <property name="visible">True</property>
                <property name="can-focus">False</property>
                <property name="margin-start">3</property>
                <property name="margin-end">3</property>
                <property name="margin-top">3</property>
                <property name="margin-bottom">3</property>
                <property name="column-homogeneous">True</property>
                <child>
                  <object class="GtkLabel">
                    <property name="visible">True</property>
                    <property name="can-focus">False</property>
                    <property name="tooltip-text" translatable="yes">The name of the tag</property>
                    <property name="halign">start</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                    <property name="label" translatable="yes">Name</property>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">1</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkEntry" id="tagging_name_entry">
                    <property name="visible">True</property>
                    <property name="can-focus">True</property>
                    <property name="tooltip-text" translatable="yes">Renaming a tag to the name of an existing tag will merge each use of this tag into that tag, then delete this tag and its description.</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">2</property>
                    <property name="width">2</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkLabel">
                    <property name="visible">True</property>
                    <property name="can-focus">False</property>
                    <property name="tooltip-text" translatable="yes">The description of the tag, what it means and what criteria should be met for it to be attached to a rating</property>
                    <property name="halign">start</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                    <property name="label" translatable="yes">Description</property>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">3</property>
                    <property name="width">2</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkScrolledWindow">
                    <property name="height-request">100</property>
                    <property name="visible">True</property>
                    <property name="can-focus">True</property>
                    <property name="shadow-type">in</property>
                    <child>
                      <object class="GtkTextView" id="tagging_description_textview">
                        <property name="visible">True</property>
                        <property name="can-focus">True</property>
                        <property name="margin-left">3</property>
                        <property name="margin-right">3</property>
                        <property name="margin-start">3</property>
                        <property name="margin-end">3</property>
                        <property name="margin-top">3</property>
                        <property name="margin-bottom">3</property>
                        <property name="wrap-mode">word</property>
                      </object>
                    </child>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">4</property>
                    <property name="width">2</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkButtonBox">
                    <property name="visible">True</property>
                    <property name="can-focus">False</property>
                    <property name="layout-style">expand</property>
                    <child>
                      <object class="GtkButton" id="attach_tag_button">
                        <property name="label" translatable="yes">Attach</property>
                        <property name="visible">True</property>
                        <property name="can-focus">True</property>
                        <property name="receives-default">True</property>
                        <property name="tooltip-text" translatable="yes">Attach this tag to the currently selected rating</property>
                        <property name="margin-start">3</property>
                        <property name="margin-end">3</property>
                        <property name="margin-top">3</property>
                        <property name="margin-bottom">3</property>
                        <signal name="clicked" handler="on_attach_tag_button_clicked" swapped="no"/>
                      </object>
                      <packing>
                        <property name="expand">True</property>
                        <property name="fill">True</property>
                        <property name="position">0</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkButton" id="detach_tag_button">
                        <property name="label" translatable="yes">Detach</property>
                        <property name="visible">True</property>
                        <property name="can-focus">True</property>
                        <property name="receives-default">True</property>
                        <property name="tooltip-text" translatable="yes">Detach this tag from the currently selected rating</property>
                        <property name="margin-start">3</property>
                        <property name="margin-end">3</property>
                        <property name="margin-top">3</property>
                        <property name="margin-bottom">3</property>
                        <signal name="clicked" handler="on_detach_tag_button_clicked" swapped="no"/>
                      </object>
                      <packing>
                        <property name="expand">True</property>
                        <property name="fill">True</property>
                        <property name="position">1</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkButton" id="vacuum_tag_button">
                        <property name="label" translatable="yes">Vacuum</property>
                        <property name="visible">True</property>
                        <property name="can-focus">True</property>
                        <property name="receives-default">True</property>
                        <property name="tooltip-text" translatable="yes">Delete all tags with zero instances</property>
                        <property name="margin-start">3</property>
                        <property name="margin-end">3</property>
                        <property name="margin-top">3</property>
                        <property name="margin-bottom">3</property>
                        <signal name="clicked" handler="on_vacuum_tag_button_clicked" swapped="no"/>
                      </object>
                      <packing>
                        <property name="expand">True</property>
                        <property name="fill">True</property>
                        <property name="position">2</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkButton" id="delete_tag_button">
                        <property name="label" translatable="yes">Delete</property>
                        <property name="visible">True</property>
                        <property name="can-focus">True</property>
                        <property name="receives-default">True</property>
                        <property name="tooltip-text" translatable="yes">Delete this tag, detaching it from all ratings</property>
                        <property name="margin-start">3</property>
                        <property name="margin-end">3</property>
                        <property name="margin-top">3</property>
                        <property name="margin-bottom">3</property>
                        <signal name="clicked" handler="on_delete_tag_button_clicked" swapped="no"/>
                      </object>
                      <packing>
                        <property name="expand">True</property>
                        <property name="fill">True</property>
                        <property name="position">3</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">6</property>
                    <property name="width">2</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkSeparator">
                    <property name="visible">True</property>
                    <property name="can-focus">False</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">5</property>
                    <property name="width">2</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkSearchEntry" id="tagging_search_entry">
                    <property name="visible">True</property>
                    <property name="can-focus">True</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                    <property name="primary-icon-name">edit-find-symbolic</property>
                    <property name="primary-icon-activatable">False</property>
                    <property name="primary-icon-sensitive">False</property>
                    <signal name="activate" handler="on_filter_tags_apply_clicked" swapped="no"/>
                    <signal name="icon-release" handler="on_filter_tags_apply_clicked" swapped="no"/>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">8</property>
                    <property name="width">2</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkScrolledWindow">
                    <property name="height-request">220</property>
                    <property name="visible">True</property>
                    <property name="can-focus">True</property>
                    <property name="shadow-type">in</property>
                    <child>
                      <object class="GtkTreeView" id="tagging_treeview">
                        <property name="visible">True</property>
                        <property name="can-focus">True</property>
                        <property name="margin-left">3</property>
                        <property name="margin-right">3</property>
                        <property name="margin-start">3</property>
                        <property name="margin-end">3</property>
                        <property name="margin-top">3</property>
                        <property name="margin-bottom">3</property>
                        <property name="activate-on-single-click">True</property>
                        <signal name="row-activated" handler="on_tagging_treeview_row_activated" swapped="no"/>
                        <child internal-child="selection">
                          <object class="GtkTreeSelection"/>
                        </child>
                      </object>
                    </child>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">9</property>
                    <property name="width">2</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkLabel" id="tagging_results_label">
                    <property name="visible">True</property>
                    <property name="can-focus">False</property>
                    <property name="halign">start</property>
                    <property name="margin-left">3</property>
                    <property name="margin-right">3</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                    <property name="label" translatable="yes"># results</property>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">7</property>
                    <property name="width">2</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkLabel" id="tagging_id_label">
                    <property name="visible">True</property>
                    <property name="can-focus">False</property>
                    <property name="halign">start</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                    <property name="label" translatable="yes">ID</property>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">0</property>
                    <property name="width">2</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkLabel" id="tagging_instances_label">
                    <property name="visible">True</property>
                    <property name="can-focus">False</property>
                    <property name="halign">end</property>
                    <property name="label" translatable="yes"># instances</property>
                  </object>
                  <packing>
                    <property name="left-attach">1</property>
                    <property name="top-attach">1</property>
                  </packing>
                </child>
              </object>
              <packing>
                <property name="position">1</property>
                <property name="tab-expand">True</property>
              </packing>
            </child>
            <child type="tab">
              <object class="GtkLabel">
                <property name="visible">True</property>
                <property name="can-focus">False</property>
                <property name="label" translatable="yes">Tagging</property>
              </object>
              <packing>
                <property name="position">1</property>
                <property name="tab-fill">False</property>
              </packing>
            </child>
            <child>
              <!-- n-columns=2 n-rows=10 -->
              <object class="GtkGrid">
                <property name="visible">True</property>
                <property name="can-focus">False</property>
                <property name="margin-start">3</property>
                <property name="margin-end">3</property>
                <property name="margin-top">3</property>
                <property name="margin-bottom">3</property>
                <property name="column-homogeneous">True</property>
                <child>
                  <object class="GtkLabel">
                    <property name="visible">True</property>
                    <property name="can-focus">False</property>
                    <property name="halign">start</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                    <property name="label" translatable="yes">Search</property>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">1</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkComboBoxText" id="filter_search_combobox">
                    <property name="name">search_combobox</property>
                    <property name="visible">True</property>
                    <property name="can-focus">False</property>
                    <property name="margin-left">3</property>
                    <property name="margin-right">3</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                    <property name="active">2</property>
                    <items>
                      <item id="0" translatable="yes">Comments</item>
                      <item id="1" translatable="yes">Tags</item>
                      <item id="2" translatable="yes">Title</item>
                      <item id="3" translatable="yes">No Filter</item>
                    </items>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">2</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkButton" id="filter_apply">
                    <property name="label" translatable="yes">Apply</property>
                    <property name="visible">True</property>
                    <property name="can-focus">True</property>
                    <property name="receives-default">True</property>
                    <property name="margin-left">3</property>
                    <property name="margin-right">3</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                    <signal name="clicked" handler="on_filter_apply_clicked" swapped="no"/>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">4</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkButton" id="filter_reset">
                    <property name="label" translatable="yes">Reset</property>
                    <property name="visible">True</property>
                    <property name="can-focus">True</property>
                    <property name="receives-default">True</property>
                    <property name="margin-left">3</property>
                    <property name="margin-right">3</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                    <signal name="clicked" handler="on_filter_reset_clicked" swapped="no"/>
                  </object>
                  <packing>
                    <property name="left-attach">1</property>
                    <property name="top-attach">4</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkLabel">
                    <property name="visible">True</property>
                    <property name="can-focus">False</property>
                    <property name="halign">start</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                    <property name="label" translatable="yes">Ratings</property>
                    <attributes>
                      <attribute name="weight" value="bold"/>
                      <attribute name="scale" value="1"/>
                      <attribute name="underline" value="True"/>
                    </attributes>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">0</property>
                    <property name="width">2</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkLabel">
                    <property name="visible">True</property>
                    <property name="can-focus">False</property>
                    <property name="halign">start</property>
                    <property name="margin-left">3</property>
                    <property name="margin-right">3</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                    <property name="label" translatable="yes">Tagging</property>
                    <attributes>
                      <attribute name="weight" value="bold"/>
                      <attribute name="scale" value="1"/>
                      <attribute name="underline" value="True"/>
                    </attributes>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">6</property>
                    <property name="width">2</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkSeparator">
                    <property name="visible">True</property>
                    <property name="can-focus">False</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">5</property>
                    <property name="width">2</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkLabel">
                    <property name="visible">True</property>
                    <property name="can-focus">False</property>
                    <property name="halign">start</property>
                    <property name="margin-left">3</property>
                    <property name="margin-right">3</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                    <property name="label" translatable="yes">Search</property>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">7</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkComboBoxText" id="filter_tags_search_combobox">
                    <property name="name">search_combobox</property>
                    <property name="visible">True</property>
                    <property name="can-focus">False</property>
                    <property name="margin-left">3</property>
                    <property name="margin-right">3</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                    <property name="active">1</property>
                    <items>
                      <item id="0" translatable="yes">Description</item>
                      <item id="2" translatable="yes">Name</item>
                      <item id="3" translatable="yes">No Filter</item>
                    </items>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">8</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkBox">
                    <property name="visible">True</property>
                    <property name="can-focus">False</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                    <property name="orientation">vertical</property>
                    <child>
                      <object class="GtkRadioButton" id="filter_tags_ascending">
                        <property name="label" translatable="yes">Ascending</property>
                        <property name="visible">True</property>
                        <property name="can-focus">True</property>
                        <property name="receives-default">False</property>
                        <property name="active">True</property>
                        <property name="draw-indicator">True</property>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">0</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkRadioButton" id="filter_tags_descending">
                        <property name="label" translatable="yes">Descending</property>
                        <property name="visible">True</property>
                        <property name="can-focus">True</property>
                        <property name="receives-default">False</property>
                        <property name="draw-indicator">True</property>
                        <property name="group">filter_tags_ascending</property>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">1</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="left-attach">1</property>
                    <property name="top-attach">8</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkLabel">
                    <property name="visible">True</property>
                    <property name="can-focus">False</property>
                    <property name="halign">start</property>
                    <property name="margin-left">3</property>
                    <property name="margin-right">3</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                    <property name="label" translatable="yes">Sort</property>
                  </object>
                  <packing>
                    <property name="left-attach">1</property>
                    <property name="top-attach">7</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkButton" id="filter_tags_apply">
                    <property name="label" translatable="yes">Apply</property>
                    <property name="visible">True</property>
                    <property name="can-focus">True</property>
                    <property name="receives-default">True</property>
                    <property name="margin-left">3</property>
                    <property name="margin-right">3</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                    <signal name="clicked" handler="on_filter_tags_apply_clicked" swapped="no"/>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">9</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkButton" id="filter_tags_reset">
                    <property name="label" translatable="yes">Reset</property>
                    <property name="visible">True</property>
                    <property name="can-focus">True</property>
                    <property name="receives-default">True</property>
                    <property name="margin-left">3</property>
                    <property name="margin-right">3</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                    <signal name="clicked" handler="on_filter_tags_reset_clicked" swapped="no"/>
                  </object>
                  <packing>
                    <property name="left-attach">1</property>
                    <property name="top-attach">9</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkBox">
                    <property name="visible">True</property>
                    <property name="can-focus">False</property>
                    <property name="orientation">vertical</property>
                    <child>
                      <object class="GtkRadioButton" id="filter_ascending">
                        <property name="label" translatable="yes">Ascending</property>
                        <property name="visible">True</property>
                        <property name="can-focus">True</property>
                        <property name="receives-default">False</property>
                        <property name="active">True</property>
                        <property name="draw-indicator">True</property>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">0</property>
                      </packing>
                    </child>
                    <child>
                      <object class="GtkRadioButton" id="filter_descending">
                        <property name="label" translatable="yes">Descending</property>
                        <property name="visible">True</property>
                        <property name="can-focus">True</property>
                        <property name="receives-default">False</property>
                        <property name="draw-indicator">True</property>
                        <property name="group">filter_ascending</property>
                      </object>
                      <packing>
                        <property name="expand">False</property>
                        <property name="fill">True</property>
                        <property name="position">1</property>
                      </packing>
                    </child>
                  </object>
                  <packing>
                    <property name="left-attach">1</property>
                    <property name="top-attach">3</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkLabel">
                    <property name="visible">True</property>
                    <property name="can-focus">False</property>
                    <property name="halign">start</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                    <property name="label" translatable="yes">Sort</property>
                  </object>
                  <packing>
                    <property name="left-attach">1</property>
                    <property name="top-attach">1</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkComboBoxText" id="filter_sort_combobox">
                    <property name="name">sort_combobox</property>
                    <property name="visible">True</property>
                    <property name="can-focus">False</property>
                    <property name="margin-left">3</property>
                    <property name="margin-right">3</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                    <property name="active">3</property>
                    <items>
                      <item id="0" translatable="yes">Score</item>
                      <item id="1" translatable="yes">Status</item>
                      <item id="2" translatable="yes">Title</item>
                    </items>
                  </object>
                  <packing>
                    <property name="left-attach">1</property>
                    <property name="top-attach">2</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkCheckButton" id="presort_checkbox">
                    <property name="label" translatable="yes">Presort Ratings</property>
                    <property name="visible">True</property>
                    <property name="can-focus">True</property>
                    <property name="receives-default">False</property>
                    <property name="tooltip-text" translatable="yes">Sort the ratings before applying the filter. This guarantees that the words are filtered in a specific order but may be slow for very large files. Otherwise, they are filtered in ascending order of ID.</property>
                    <property name="draw-indicator">True</property>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">3</property>
                  </packing>
                </child>
              </object>
              <packing>
                <property name="position">2</property>
                <property name="tab-expand">True</property>
              </packing>
            </child>
            <child type="tab">
              <object class="GtkLabel">
                <property name="visible">True</property>
                <property name="can-focus">False</property>
                <property name="label" translatable="yes">Filter</property>
              </object>
              <packing>
                <property name="position">2</property>
                <property name="tab-fill">False</property>
              </packing>
            </child>
            <child>
              <!-- n-columns=3 n-rows=4 -->
              <object class="GtkGrid">
                <property name="visible">True</property>
                <property name="can-focus">False</property>
                <property name="column-homogeneous">True</property>
                <child>
                  <object class="GtkEntry" id="filepath_entry">
                    <property name="visible">True</property>
                    <property name="can-focus">True</property>
                    <property name="margin-left">3</property>
                    <property name="margin-right">3</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">1</property>
                    <property name="width">3</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkLabel">
                    <property name="visible">True</property>
                    <property name="can-focus">False</property>
                    <property name="halign">start</property>
                    <property name="margin-left">3</property>
                    <property name="margin-right">3</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                    <property name="label" translatable="yes">File Location</property>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">0</property>
                    <property name="width">3</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkFileChooserButton" id="file_chooser_button">
                    <property name="visible">True</property>
                    <property name="can-focus">False</property>
                    <property name="margin-left">3</property>
                    <property name="margin-right">3</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                    <property name="action">select-folder</property>
                    <property name="title" translatable="yes"/>
                    <signal name="selection-changed" handler="on_file_chooser_button_file_set" swapped="no"/>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">3</property>
                    <property name="width">3</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkButton" id="open_file_button">
                    <property name="label" translatable="yes">Open</property>
                    <property name="visible">True</property>
                    <property name="can-focus">True</property>
                    <property name="receives-default">True</property>
                    <property name="margin-left">3</property>
                    <property name="margin-right">3</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                    <signal name="clicked" handler="on_open_file_button_clicked" swapped="no"/>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">2</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkButton" id="new_file_button">
                    <property name="label" translatable="yes">New</property>
                    <property name="visible">True</property>
                    <property name="can-focus">True</property>
                    <property name="receives-default">True</property>
                    <property name="margin-left">3</property>
                    <property name="margin-right">3</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                    <signal name="clicked" handler="on_new_file_button_clicked" swapped="no"/>
                  </object>
                  <packing>
                    <property name="left-attach">2</property>
                    <property name="top-attach">2</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkButton" id="save_file_button">
                    <property name="label" translatable="yes">Save</property>
                    <property name="visible">True</property>
                    <property name="can-focus">True</property>
                    <property name="receives-default">True</property>
                    <property name="margin-left">3</property>
                    <property name="margin-right">3</property>
                    <property name="margin-start">3</property>
                    <property name="margin-end">3</property>
                    <property name="margin-top">3</property>
                    <property name="margin-bottom">3</property>
                    <signal name="clicked" handler="on_save_file_button_clicked" swapped="no"/>
                  </object>
                  <packing>
                    <property name="left-attach">1</property>
                    <property name="top-attach">2</property>
                  </packing>
                </child>
              </object>
              <packing>
                <property name="position">3</property>
                <property name="tab-expand">True</property>
              </packing>
            </child>
            <child type="tab">
              <object class="GtkLabel">
                <property name="visible">True</property>
                <property name="can-focus">False</property>
                <property name="label" translatable="yes">Files</property>
              </object>
              <packing>
                <property name="position">3</property>
                <property name="tab-fill">False</property>
              </packing>
            </child>
          </object>
          <packing>
            <property name="resize">True</property>
            <property name="shrink">True</property>
          </packing>
        </child>
      </object>
    </child>
  </object>
</interface>
'''
TAG_HELP = '''Guide to Searching by Tags
==========================

## Overview ##

A search using tags takes the form of an expression in the tag selection
language, the form and usage of which is described in this document.

### Expressions ###

The tag selection language is composed of expressions. An expression consists
of a series of tag and selection operators applied to those tags. In place of
a tag, one can also use a nested expression. Nested expressions begin with the
**{** character and end with the **}** character.

## Tags ##

Tags are little descriptive strings of text that are attached to items to
describe a quality of that item. Tags begin and end with a non-whitepace
character, but can contain whitespace inside of them. Tags cannot contain
a reserved character, unless that character is escaped. Otherwise, the presence
of a reserved character signifies a boundary of the tag.

### Reserved Characters ###

The set of reserved characters, displayed with each character separated by a
space, is as follows: **~ , ^ | ? =**

### White Space ###

The term "whitespace" refers to invisible characters such as spaces, tabs,
newlines, and carriage returns.  

Whitespace characters that are inside of tag names are treated as part of that
name, and are interpreted as is. For a tag with the identifier **tag name**, the
space between the words **tag** and **name** is part of the name itself. Thus the
tag **tag name** is different and distinct from the tag **tag   name**.

Whitespace characters that are between tag names and operators are ignored. So
given the expression **left | right**, the spaces around **|** are ignored. This
means that following are all equivalent: **left|right** and **left| right** and
**  left   |right   **

### Escaping Characters ###

In order to inlcude a reserved character in a tag, that character must be
preceeded by the escape character **\\** as in the following **tag with\\, comma**,
in this case, the comma is part of the tag name and it is interpreted as being
**tag with, comma**. Non-reserved characters preceeded by the escape character
are treated as is, so **a\p\p\le** is treated the same as **apple**. In order to
include a literal **\\** in a tag name, it must also be preceeded by an escape
character as in **literal \\\\ character** which is read as **literal \\ character**

## Operators ##

### Precedence ###

In the absence of braces the operators are evaluated in the following order,
starting from the lowest number and proceeding to the highest:

	1. ~
	2. ,
	3. ^
	4. |
	5. ?
	6. =

### Unary Operators ###

There is only one unary operator, the logical not operator. It takes only one
operand to its right, in the form **\[operator\] \[operand\]**

#### The Not Operator ####

The Not operator **~** selects the items that are not selected by the expression
on its right hand side. Items that are selected by the expression on its right
hand side are filtered out.

### Binary Operators ###

There are five binary operators. They each take two operands, one to the left
and one to the right, in the form **\[operand A\] \[operator\] \[operand B\]**

Binary operators are either commutative or non-commutative. For a commutative
operator, the expression  **\[A\] \[operator\] \[B\]** is eqivalent to the
expression **\[B\] \[operator\] \[B\]** but the same is not true for a
non-commutative operator.

#### The And Operator ####

The And operator **,** selects items that are selected by both the expression on
its right hand side as well as the expression on its left hand side. Items only
selected by one expression or by neither are filtered out.

The And operator is commutative.

### The Exclusive Or Operator ####

The Exclusive Or operator **^** selects items that are selected by either the
expression on its right hand side or the expression on its left hand side, but
not by both. Items selected by both expression and items selected by neither
expression are filtered out.

The Exclusive Or operator is commutative.

#### The Inclusive Or Operator ####

The Inclusive Or operator **|** selects items that are selected by the expression
on its right hand side, by the expression on its left hand side, or by both of
these expressions. Items not selected by either expression are filtered out.

The Inclusive Or operator is commutative.

#### The Only If Operator ####

The Only If operator **?** works slightly differently from the other binary
operators. It selects all items not selected by the expression on its left hand
side, and for the items that are selected by the expression on its left hand
side, it selects only those items that are also selected by its right hand side.
It filters out items that are selected by its left hand side by not its right
hand side.

The Only If operator is non-commutative.

#### The Equivalence Operator ####

The Equivalence Operator **=** selects items that are either selected by both the
expression on its right hand side and its left hand side, or by neither of these
expressions. Items only selected by one side are filtered out.

The Equivalence operator is commutative.

#### The Statement Operator ####

The Statement Operator **;** select the items selected by the expression on its 
right hand side, ignoring the result of the expression on its left hand side.
This is useful for use with predicates that have side-effects, as a way to apply
them specifically for those side-effects.

The Statement Operator is non-commutative.

## Examples ##

The expression **dogs, \{beach? crabs ^ seagulls\}** selects every picture with
a dog, and for those on a beach, filters out those without any crabs or seagulls
and those that have both crabs and seagulls.

The expression **fantasy = animation | novel** selects every story that is in
the fantasy genre that is either an animation or a novel or both and every
non-fantasy work that is neither an animation nor a novel.
'''