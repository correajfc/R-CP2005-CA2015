

//Google Script para procesar las tablas en excel que arroja Redatam Censo 2005 del DANE
//Los archivos de excel se importaron a hojas de cálculo de google y ahí se procesaron con estas funciones

//variable sobre personas

function tabularEdadPromedio() {
  var ss = SpreadsheetApp.getActiveSpreadsheet();
  SpreadsheetApp.setActiveSheet(ss.getSheetByName("personas"));
var sheet = SpreadsheetApp.getActiveSheet();
  
var data=sheet.getDataRange().getValues();
  
var numRows = data.length;
var newData = new Array();
var i=2;
var j=2;  
  
  do
  {
  var strSplit =data[i][0].split("#");
    
    if(strSplit[0].trim()=="AREA")
    { 
    var newRow=[];
    newRow[0]=strSplit[1].trim();
      if(data[i+j][0]!="Tabla vacía"){
        j++;
        newRow[1] =  data[i+j][1];
        newRow[2] =  data[i+j][2];
        newData.push(newRow);
        
        i=i+j;
        j=2; 
      }
    
      
    }
 
  i++;
  }
  while (i<numRows); 

  
  //Guardar los datos en una nueva hoja 
  
  //var j=sheet.getIndex();
  SpreadsheetApp.setActiveSheet(ss.getSheetByName("t_persona_edad"));
  //var ss = SpreadsheetApp.getActiveSpreadsheet();
  //ss.insertSheet('listaArtistas',j+1 );
   // Set up the spreadsheet to display the results
  var headers = [["su_id","personas","edad_promedio"]];
  var sheetindicadores=SpreadsheetApp.getActiveSheet();
  sheetindicadores.clear();
  sheetindicadores.getRange("A1:C1").setValues(headers);
  sheetindicadores.getRange(2, 1, newData.length, newData[0].length).setValues(newData);
  
  
}

function tabularEtniaPersonas() {
  var ss = SpreadsheetApp.getActiveSpreadsheet();
  SpreadsheetApp.setActiveSheet(ss.getSheetByName("etnia"));
var sheet = SpreadsheetApp.getActiveSheet();
  
var data=sheet.getDataRange().getValues();
  
var numRows = data.length;
var newData = new Array();
var i=2;
var j=2;  
  
  do
  {
  var strSplit =data[i][0].split("#");
    
    if(strSplit[0].trim()=="AREA")
    { 
    var newRow=[];
    newRow[0]=strSplit[1].trim();
      if(data[i+j][0]=="Categorías"){
        while (data[i+j][0] != "Total"){
          switch (data[i+j][0])  {
            case "Indígena":
              newRow[1] = data[i+j][1];
              break;
            case "Rom":
              newRow[2] = data[i+j][1];
              break;
            case "Raizal de San Andrés y Providencia":
              newRow[3] = data[i+j][1];
              break;
            case "Palenquero":
              newRow[4] = data[i+j][1];
              break;
            case "Negro (a), mulato, afrocolombiano":
              newRow[5] = data[i+j][1];
              break;
            case "Ninguno de los anteriores":
              newRow[6] = data[i+j][1];
              break;
            case "No Informa":
              newRow[7] = data[i+j][1];
              break;
   
          }
          j++;
      
    }
     newRow[8] = data[i+j][1];
     newData.push(newRow);
     i=i+j; 
     j=2;
      
    }
    
    }
  i++;
  }
  while (i<numRows); 

  
  //Guardar los datos en una nueva hoja 
  
  //var j=sheet.getIndex();
  SpreadsheetApp.setActiveSheet(ss.getSheetByName("t_etnia"));
  //var ss = SpreadsheetApp.getActiveSpreadsheet();
  //ss.insertSheet('listaArtistas',j+1 );
   // Set up the spreadsheet to display the results
  var headers = [["su_id","indigena","rom","raizal_SAI_Providencia","palenquero","negro_mulato_afrocolombiano","ninguno_de_los_anteriores","no_informa","total_personas"]];
  var sheetindicadores=SpreadsheetApp.getActiveSheet();
  sheetindicadores.clear();
  sheetindicadores.getRange("A1:I1").setValues(headers);
  sheetindicadores.getRange(2, 1, newData.length, newData[0].length).setValues(newData);
  
  
}



function tabularEducacionSuperior() {
  var ss = SpreadsheetApp.getActiveSpreadsheet();
  SpreadsheetApp.setActiveSheet(ss.getSheetByName("nivel_estudios"));
var sheet = SpreadsheetApp.getActiveSheet();
  
var data=sheet.getDataRange().getValues();
  
var numRows = data.length;
var newData = new Array();
var i=2;
var j=2;  
  
  do
  {
  var strSplit =data[i][0].split("#");
    
    if(strSplit[0].trim()=="AREA")
    { 
    var newRow=[];
    newRow[0]=strSplit[1].trim();
      if(data[i+j][0]=="Categorías"){
        while (data[i+j][0] != "Total"){
          switch (data[i+j][0])  {
            case "Superior y postgrado":
              newRow[1] = data[i+j][1];
              break;
            case "Ninguno":
              newRow[2] = data[i+j][1];
              break;
            
   
          }
          j++;
      
    }
     newRow[3] = data[i+j][1];
     newData.push(newRow);
     i=i+j; 
     j=2;
      
    }
    
    }
  i++;
  }
  while (i<numRows); 

  
  //Guardar los datos en una nueva hoja 
  
  //var j=sheet.getIndex();
  SpreadsheetApp.setActiveSheet(ss.getSheetByName("t_nivel_estudios"));
  //var ss = SpreadsheetApp.getActiveSpreadsheet();
  //ss.insertSheet('listaArtistas',j+1 );
   // Set up the spreadsheet to display the results
  var headers = [["su_id","superior_postgrado","ningun_estudio","total_personas"]];
  var sheetindicadores=SpreadsheetApp.getActiveSheet();
  sheetindicadores.clear();
  sheetindicadores.getRange("A1:D1").setValues(headers);
  sheetindicadores.getRange(2, 1, newData.length, newData[0].length).setValues(newData);
  
  
}

function tabularAlgunaLimitacion() {
  var ss = SpreadsheetApp.getActiveSpreadsheet();
  SpreadsheetApp.setActiveSheet(ss.getSheetByName("limitacion"));
var sheet = SpreadsheetApp.getActiveSheet();
  
var data=sheet.getDataRange().getValues();
  
var numRows = data.length;
var newData = new Array();
var i=2;
var j=2;  
  
  do
  {
  var strSplit =data[i][0].split("#");
    
    if(strSplit[0].trim()=="AREA")
    { 
    var newRow=[];
    newRow[0]=strSplit[1].trim();
      if(data[i+j][0]=="Categorías"){
        while (data[i+j][0] != "Total"){
          switch (data[i+j][0])  {
            case "SI":
              newRow[1] = data[i+j][1];
              break;
            case "NO":
              newRow[2] = data[i+j][1];
              break;
            
   
          }
          j++;
      
    }
     newRow[3] = data[i+j][1];
     newData.push(newRow);
     i=i+j; 
     j=2;
      
    }
    
    }
  i++;
  }
  while (i<numRows); 

  
  //Guardar los datos en una nueva hoja 
  
  //var j=sheet.getIndex();
  SpreadsheetApp.setActiveSheet(ss.getSheetByName("t_limitacion"));
  //var ss = SpreadsheetApp.getActiveSpreadsheet();
  //ss.insertSheet('listaArtistas',j+1 );
   // Set up the spreadsheet to display the results
  var headers = [["su_id","SI","NO","total_personas"]];
  var sheetindicadores=SpreadsheetApp.getActiveSheet();
  sheetindicadores.clear();
  sheetindicadores.getRange("A1:D1").setValues(headers);
  sheetindicadores.getRange(2, 1, newData.length, newData[0].length).setValues(newData);
  
  
}


// viviendas

function tabularOcupacionViviendas() {
  var ss = SpreadsheetApp.getActiveSpreadsheet();
  SpreadsheetApp.setActiveSheet(ss.getSheetByName("ocupacion_viviendas"));
var sheet = SpreadsheetApp.getActiveSheet();
  
var data=sheet.getDataRange().getValues();
  
var numRows = data.length;
var newData = new Array();
var i=2;
var j=2;  
  
  do
  {
  var strSplit =data[i][0].split("#");
    
    if(strSplit[0].trim()=="AREA")
    { 
    var newRow=[];
    newRow[0]=strSplit[1].trim();
      if(data[i+j][0]=="Categorías"){
        while (data[i+j][0] != "Total"){
          switch (data[i+j][0])  {
            case "Ocupada con personas presentes":
              newRow[1] = data[i+j][1];
              break;
            case "Ocupada con personas ausentes":
              newRow[2] = data[i+j][1];
              break;
            case "Desocupadas":
              newRow[3] = data[i+j][1];
              break;
            case "Desocupada por Uso Temporal":
              newRow[4] = data[i+j][1];
              break;
              
          }
          j++;
      
    }
     newRow[5] = data[i+j][1];
     newData.push(newRow);
     i=i+j; 
     j=2;
      
    }
    
    }
  i++;
  }
  while (i<numRows); 

  
  //Guardar los datos en una nueva hoja 
  
  //var j=sheet.getIndex();
  SpreadsheetApp.setActiveSheet(ss.getSheetByName("t_ocupacion_viviendas"));
  //var ss = SpreadsheetApp.getActiveSpreadsheet();
  //ss.insertSheet('listaArtistas',j+1 );
   // Set up the spreadsheet to display the results
  var headers = [["su_id","ocupada_con_personas_presentes","ocupada_con_personas_ausentes","desocupadas","desocupada_por_uso_temporal","total_viviendas"]];
  var sheetindicadores=SpreadsheetApp.getActiveSheet();
  sheetindicadores.clear();
  sheetindicadores.getRange("A1:F1").setValues(headers);
  sheetindicadores.getRange(2, 1, newData.length, newData[0].length).setValues(newData);
  
  
}

function tabularTipoViviendas() {
  var ss = SpreadsheetApp.getActiveSpreadsheet();
  SpreadsheetApp.setActiveSheet(ss.getSheetByName("tipo_vivienda"));
var sheet = SpreadsheetApp.getActiveSheet();
  
var data=sheet.getDataRange().getValues();
  
var numRows = data.length;
var newData = new Array();
var i=2;
var j=2;  
  
  do
  {
  var strSplit =data[i][0].split("#");
    
    if(strSplit[0].trim()=="AREA")
    { 
    var newRow=[];
    newRow[0]=strSplit[1].trim();
      if(data[i+j][0]=="Categorías"){
        while (data[i+j][0] != "Total"){
          switch (data[i+j][0])  {
            case "Casa":
              newRow[1] = data[i+j][1];
              break;
            case "Casa indígena":
              newRow[2] = data[i+j][1];
              break;
            case "Apartamento":
              newRow[3] = data[i+j][1];
              break;
            case "Tipo cuarto":
              newRow[4] = data[i+j][1];
              break;
            case "Otro tipo de vivienda":
              newRow[5] = data[i+j][1];
              break;
              
              
          }
          j++;
      
    }
     newRow[6] = data[i+j][1];
     newData.push(newRow);
     i=i+j; 
     j=2;
      
    }
    
    }
  i++;
  }
  while (i<numRows); 

  
  //Guardar los datos en una nueva hoja 
  
  //var j=sheet.getIndex();
  SpreadsheetApp.setActiveSheet(ss.getSheetByName("t_tipo_vivienda"));
  //var ss = SpreadsheetApp.getActiveSpreadsheet();
  //ss.insertSheet('listaArtistas',j+1 );
   // Set up the spreadsheet to display the results
  var headers = [["su_id","casa","casa_indigena","apartamento","tipo_cuarto","otro_tipo_de_vivienda","total_viviendas"]];
  var sheetindicadores=SpreadsheetApp.getActiveSheet();
  sheetindicadores.clear();
  sheetindicadores.getRange("A1:G1").setValues(headers);
  sheetindicadores.getRange(2, 1, newData.length, newData[0].length).setValues(newData);
  
  
}

function tabularUsoViviendas() {
  var ss = SpreadsheetApp.getActiveSpreadsheet();
  SpreadsheetApp.setActiveSheet(ss.getSheetByName("uso_predios"));
var sheet = SpreadsheetApp.getActiveSheet();
  
var data=sheet.getDataRange().getValues();
  
var numRows = data.length;
var newData = new Array();
var i=2;
var j=2;  
  
  do
  {
  var strSplit =data[i][0].split("#");
    
    if(strSplit[0].trim()=="AREA")
    { 
    var newRow=[];
    newRow[0]=strSplit[1].trim();
      if(data[i+j][0]=="Categorías"){
        while (data[i+j][0] != "Total"){
          switch (data[i+j][0])  {
            case "Uso Vivienda":
              newRow[1] = data[i+j][1];
              break;
            case "Uso Unidad Económica":
              newRow[2] = data[i+j][1];
              break;
            case "Uso LEA":
              newRow[3] = data[i+j][1];
              break;
            
          }
          j++;
      
    }
     newRow[4] = data[i+j][1];
     newData.push(newRow);
     i=i+j; 
     j=2;
      
    }
    
    }
  i++;
  }
  while (i<numRows); 

  
  //Guardar los datos en una nueva hoja 
  
  //var j=sheet.getIndex();
  SpreadsheetApp.setActiveSheet(ss.getSheetByName("t_uso_predios"));
  //var ss = SpreadsheetApp.getActiveSpreadsheet();
  //ss.insertSheet('listaArtistas',j+1 );
   // Set up the spreadsheet to display the results
  var headers = [["su_id","uso_vivienda","uso_unidad_economica","uso_LEA","total_viviendas"]];
  var sheetindicadores=SpreadsheetApp.getActiveSheet();
  sheetindicadores.clear();
  sheetindicadores.getRange("A1:E1").setValues(headers);
  sheetindicadores.getRange(2, 1, newData.length, newData[0].length).setValues(newData);
  
  
}



