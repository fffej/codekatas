namespace MemoryMappedDatabase
{
    struct BoolValue : IColumnValue
    {
        private readonly bool m_Value;

        public BoolValue(bool i)
        {
            m_Value = i;
        }
    }
}