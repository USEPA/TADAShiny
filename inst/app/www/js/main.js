/*
$.extend($.fn.dataTable.defaults, {
    responsive: true
} );
 
$(document).ready(function() {
    $('#summary').DataTable();
});
*/

$(document).ready(function() {
    $('#summary').DataTable( {
        responsive: true,
        columnDefs: [
            { responsivePriority: 1, targets: 12 },
        ]
    } );
} );

function activeTab(tab){
  $('.nav-tabs a[href="#' + tab + '"]').tab('show');
};

function hideTabs(tab){
  document.getElementById("tab_2").classList.add("disabled")
  document.getElementById("tab_3").classList.add("disabled")
};

function showTabs(tab){
  document.getElementById("tab_2").classList.remove("disabled")
  document.getElementById("tab_3").classList.remove("disabled")
};