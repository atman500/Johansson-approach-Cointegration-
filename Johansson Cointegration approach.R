#**************************University of Eloue********************************
#*************************opensource softwars*********************************
#************************accounting and Odit *********************************
#***********************Dr .medini Atmane ************************************
#***********************acadimic year 2025_2026*******************************

#Importation of dataset************************************

data=read.csv(file.choose(),header = TRUE,sep = ";")
names(data)
attach(data)
print(head(data))

#requirement of Packages******************************************************
require(tseries)
require(vars)
require(timeSeries)
require(urca)
require(ggplot2)

#Make Transformation of the  data into  Time series*************************

GDP_index=ts(GDP_Index,frequency = 365,start = c(2000,1))
Inflation_Rate=ts(Inflation_Rate,frequency = 365,start = c(2000,1))
Interest_Rate=ts(Interest_Rate,frequency = 365,start = c(2000,1))
Exchange_Rate=ts(Exchange_Rate,frequency = 365,start = c(2000,1))
Stock_Market_Index=ts(Stock_Market_Index,frequency = 365,start = c(2000,1)) 

# ======================================================================
# 7. منهجية جوهانسون للتكامل المشترك (Johansen Cointegration Methodology)
# ======================================================================

# *** ملاحظة هامة: نستخدم السلاسل في مستوياتها الأصلية (Level) وليس الفروق ***

# ---------------------------------------------------------
# الخطوة 1: دمج السلاسل الزمنية في نظام مصفوفة واحد (System)
# ---------------------------------------------------------
# في جوهانسون، لا يوجد تابع ومستقل. نجمع المتغيرات التي نريد دراستها.
# سنختار 3 متغيرات كمثال: الناتج المحلي، سعر الصرف، ومؤشر السوق المالي
johansen_data <- cbind(GDP_index, Exchange_Rate, Stock_Market_Index)

# حذف أي قيم مفقودة (NA) قد تنتج عن عملية الدمج لتجنب أخطاء التقدير
johansen_data <- na.omit(johansen_data) 

print(head(johansen_data))

# ---------------------------------------------------------
# الخطوة 2: تحديد فترات الإبطاء المثلى (Optimal Lags Selection)
# ---------------------------------------------------------
# دالة VARselect تختبر فترات الإبطاء من 1 إلى الحد الأقصى (مثلا 10)
# ---------------------------------------------------------
# الخطوة 2: تحديد فترات الإبطاء المثلى (Optimal Lags Selection)
# ---------------------------------------------------------
lag_selection <- VARselect(johansen_data, lag.max = 10, type = "const")

cat("\n*** معايير اختيار فترات الإبطاء المثلى ***\n")
print(lag_selection$selection)

# سحب فترة الإبطاء وفقا لمعيار AIC
optimal_lag <- lag_selection$selection["AIC(n)"]
print(optimal_lag)

# التعديل الهام: التأكد من أن فترة الإبطاء لا تقل عن 2 لتلبية شروط دالة ca.jo
if(optimal_lag < 2) {
  cat("\nملاحظة: معيار AIC اختار فترة إبطاء أقل من 2. تم رفعها آليا إلى 2 لتلبية شرط دالة Johansen.\n")
  optimal_lag <- 2
} else {
  optimal_lag <- optimal_lag
}

print(paste("فترة الإبطاء المعتمدة في الاختبار هي:", optimal_lag))

# ---------------------------------------------------------
# الخطوة 3: تطبيق اختبار جوهانسون (Johansen Test)
# ---------------------------------------------------------
# أ. اختبار إحصائية الأثر (Trace Statistic)
johansen_trace <- ca.jo(johansen_data, type = "trace", ecdet = "const", K = optimal_lag)

summary(johansen_trace)

# ب. اختبار إحصائية القيمة الذاتية العظمى (Maximum Eigenvalue)
johansen_eigen <- ca.jo(johansen_data, type = "eigen", ecdet = "const", K = optimal_lag)

print(johansen_eigen)


# عرض النتائج
cat("\n=======================================================\n")
cat("          نتائج اختبار الأثر (Trace Test)              \n")
cat("=======================================================\n")
summary(johansen_trace)

cat("\n=======================================================\n")
cat("   نتائج اختبار القيمة الذاتية (Max-Eigenvalue Test)   \n")
cat("=======================================================\n")
summary(johansen_eigen)




# لاستخراج جدول القيم الحرجة لاختبار الأثر
print(johansen_trace@cval)

# لاستخراج جدول القيم الحرجة لاختبار القيمة العظمى
print(johansen_eigen@cval)